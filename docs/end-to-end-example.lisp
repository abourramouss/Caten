;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(ql:quickload :caten)

(defpackage :end-to-end
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm :caten/codegen/expr-cache)
  ;; Import some low-level APIs
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules
   #:schedule-item-write-define-global)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/renderer
   #:CStyle-Renderer
   #:%render-kernel
   #:%compile-kernel)
  (:import-from
   :caten/codegen/jit
   #:schedule-graph->avm-graph))
(in-package :end-to-end)

;;; 1. High-Level API

;;; In this example, we will show an end-to-end example of how the overall process of code generation works, starting from Caten's
;;; High-Level Interface (caten/apis)

;;; Let's first define a class (similar to Torch's module) for the function sin(cos(x))
;;; It will be the same class used in getting-started.lisp
(defclass SinCos (Func) nil
  (:documentation "The func SinCos computes sin(cos(x))"))

(defmethod forward ((op SinCos) &rest tensors)
  ;; Defines the forward pass computation: sin(cos(x))
  (st "A[~] -> A[~]" (tensors)))

(defmethod backward ((op SinCos) &optional prev-grad)
  ;; Defines the backward pass (gradient computation), ignored in this example
  (declare (ignore prev-grad))
  nil)

(defmethod lower ((op SinCos) &rest inputs)
  ;; Lowers the high-level operation into intermediate representation
  (let ((x (car inputs)))
    (with-context
        (a (%sin (%add x (%fconst (/ pi 2)))))
      (b (%sin a)))))

;;; You can create the next lazy tensor using the forward method.
;;; Let's define a function doing this as a utility named !sincos.
(defun !sincos (tensor)
  (forward (make-instance 'SinCos) tensor))

;;; First of all, let's initialize the module we just defined !sincos
(defparameter mytensor (make-tensor '(1) :initial-element 1.0))

(defparameter sincos (!sincos mytensor))

;;; If we print, we will see what the contents of sincos are.
(print sincos)
;;; It might surprise you that there is nothing, the reason is that Caten is lazy, we have to compile the expression into
;;; an abstract syntax tree. In order to compile, we can use the caten function as follows:

;;; 2. Intermediate Representation
(defparameter graph (caten sincos))

;;; This will generate an AVM graph as seen in the REPL output.
;;; The graph generated is an intermediate representation.
(print graph)

;;; We defined SinCos lower as:
;;; (a (%sin (%add x (%fconst (/ pi 2)))))
;;; (b (%sin a))
;;; and in the intermediate representation we can map the operations to:
;;; 
;;; <ALLOCATE : TID1421 <- (shape=(1), stride=(1)) ...> -> will prepare the space to load the value of 1.0
;;; <Node[BUFFER] LOAD(NID1436) : LID1435* <- (TID1421) where :value=1.0> -> the tensor value is loaded into the allocated space
;;; <Node[BUFFER] ALLOCATE(NID1439) : SID1438* <- () -> allocates space for the defined const / pi 2
;;; <Node[BUFFER] LOAD(NID1441) : LID1440* <- (SID1438) where :value=1.5707964> -> loads the constant into a buffer
;;; <Node[BINARYOPS] ADD(NID1443) : BID1442* <- (LID1435, LID1440)> -> performs the addition (%add x (%fconst (/ pi 2))
;;; <Node[UNARYOPS] SIN(NID1445) : UID1444* <- (BID1442)> -> applies the sin function to the previous operation (%sin (%add x (%fconst (/ pi 2))))
;;; <Node[UNARYOPS] SIN(NID1447) : UID1446* <- (UID1444)> -> applies sin again, now to a (b (%sin a)
;;; <Node[SPECIAL/VM] PAUSE/BACKWARD(NID1460) : STC1423_1* <- (UID1446)> -> synchronizes the graph, let's call it a sink node, the output node b

;;; This is it as far as the Intermediate Representation goes.

;;; 3. Code Generation

;;; The goal of run-type-infer is to propagate the type and shape information through the graph
;;; Executes vm/forward over the graph to propagate the information forward.
;;; It simulates running the graph forward and backward to infer the types of the nodes.
(print (run-type-infer graph))

;;; The goal is to transform and rename parts of the graph to improve readability, consistency or prepare it for code generation and optimizations.
;;; It takes the raw AVM graph, rewrites certain references, applies stable naming conventions and returns a more uniform and readable graph representation.
(print (apply-rewriting-rules graph))

;;; The resulting graph after run-type-infer and apply-rewriting-rules is the following:
;;; <ALLOCATE : val_0 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE ((1)) <- NIL> :_read_views=(NIL NIL) :_output_type=NIL>
;;; <Node[BUFFER] LOAD(NID1588) : val_1* <- (val_0) where :value=1.0 :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;; <Node[BUFFER] ALLOCATE(NID1591) : val_2* <- () where :nrank=0 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE (NIL) <- NIL>>
;;; <Node[BUFFER] LOAD(NID1593) : val_3* <- (val_2) where :value=1.5707964 :_type_relay=#<INFERRED-TYPE (NIL) <- (NIL)> :_read_views=(NIL)>
;;; <Node[BINARYOPS] ADD(NID1595) : val_4* <- (val_1, val_3) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1) NIL)> :_read_views=(NIL NIL)>
;;; <Node[UNARYOPS] SIN(NID1597) : val_5* <- (val_4) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;; <Node[UNARYOPS] SIN(NID1599) : val_6* <- (val_5) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;; <Node[SPECIAL/VM] PAUSE/BACKWARD(NID1612) : val_7* <- (val_6) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>

;;; Compared to the initial compiled graph, variables are renamed to val_X. It makes the graph more human
;;; readable and easier to debug. It also ensures type inference information (_type_relay, INFERRED-TYPE)
;;; is attached to the nodes of the graph and clear.
;;; It simplifies the IR for debugging and inspection. To make it easier in the code generation step.

;;; graph-schedule generates a fast graph from the avm-graph, which is a wrapper around the graph we had earlier.
;;; 1. First of all, we wrap the graph around an AVM graph.
;;; 2. Then the graph-schedule function is called over it.
;;;    graph-schedule does:
;;;    1. Creates a schedule context and calls graph-breadth-first-schedule
;;;       1.1 It first traverses the graph in a BFS (breadth-first) fashion
;;;       1.2 Groups nodes together into larger "fused" kernels (optimizations)
;;;       1.3 Distributes certain operations like load that makes resulting kernels simpler and more uniform.
;;;    2. Then we try to merge groups of nodes into bigger, more optimal kernels.
;;;       2.1 If two parts of a graph share operations, and they can be combined without violating constraints,
;;;           they are merged
;;;       2.2 Extra rewriting is applied so data shapes, broadcasting and reduction dimensions align properly.
;;;    3. Finally, the fast graph is generated. A fast graph is a more optimized data structure for the final scheduler graph,
;;;       it assumes a Directed Acyclic Graph structure and uses hash-table-based lookup for nodes to speed up further operations.
;;;       3.1 The final FastGraph is a cleaner, more optimized representation of the computation.
;;;       3.2 It contains fewer, more meaningful nodes. For instance, SIN operations are combined into a single kernel.
;;;           It's also easier and more efficient to traverse and manipulate, which makes it suitable for code generation.

(defparameter scheduled-graph (graph-schedule (avm-graph graph)))

(print scheduled-graph)
;;; FastGraph[seen=NIL, outputs=(val_7)] {
;;; { Allocate } : [ val_0 <- (1) where lowered-p=nil ]
;;; { Allocate } : [ val_2 <- NIL where lowered-p=nil ]
;;; {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD1669]
;;; {   VMOP   } : [ val_7 <- val_6 where lowered-p=nil :name=FUSED_BACKWARD1665]
;;; }

;;; In the FastGraph, the operations we had previously such as add, sin, have been replaced by a fused kernel
;;; We had a sequence LOAD -> ADD -> SIN -> SIN that is converted into { KERNEL } : [ val_6 <- val_2, val_0 ... :name=FUSED_SIN_SIN_ADD_LOAD_LOAD1669]
;;; The identifiers we had in the graph were converted to val_X: TID1421 (original) -> val_0 (rewritten)

;;; Now we can extract each graph node from the FastGraph, this is where it gets really interesting, and where the actual code generation happens

;;; First we extract the nodes from the FastGraph we had previously created
;;; graph-nodes is a method from FastGraph, it returns a list of unique nodes from FastGraph.
(defparameter list-of-graph-nodes (graph-nodes scheduled-graph))

(print list-of-graph-nodes)
;;; Processing (PRINT LIST-OF-GRAPH-NODES)
;;; ({ Allocate } : [ val_0 <- (1) where lowered-p=nil ]
;;;  {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD964]
;;;  { Allocate } : [ val_2 <- NIL where lowered-p=nil ]
;;;  {   VMOP   } : [ val_7 <- val_6 where lowered-p=nil :name=FUSED_BACKWARD960])

;;; Here we test whether an item of the FastGraph is jitable or not. Jitable items are the ones lowered.
(dolist (item list-of-graph-nodes)
  (print item)
  (format t "~A~%" (getattr item :jitable)))

;;; Jitable items (which at this point we can call them schedule-items) are items in the computation graph that can be just-in-time compiled.

;;; JIT compilation is the process of translating intermediate representations into lower-level optimized machine code (in this case blueprint code and later C kernels).

;;; A schedule-item that is jitable meets the criteria for being compiled into a kernel or blueprint suitable code.

;;; In this case, the only schedule-item that has jitable set to true is the fused kernel!
;;; {  KERNEL  } : [ val_6 <- val_2, val_0 where lowered-p=nil :name=FUSED_SIN_SIN_ADD_LOAD_LOAD964] T

;;; Now, we can take the schedule-item and lower it

;;; Retrieve the second node in the list of graph nodes
(defparameter item1 (nth 1 list-of-graph-nodes))

;;; Print the node to see what it is before lowering.
;;; At this stage, `item1` is a scheduled graph node—likely a kernel node 
;;; that can be lowered into a blueprint.
(print item1)

;;; What does lower-schedule-item do?
;;; ---------------------------------
;;; `lower-schedule-item` takes a Schedule-Item node and:
;;; 1. Checks if it is jitable (i.e., suitable for JIT compilation).
;;; 2. If yes, it performs a process called 'lowering':
;;;    - Extracts the computation graph within this schedule-item.
;;;    - Determines iteration spaces and common loop structures for the operations inside it.
;;;    - Converts the fused operations into a low-level blueprint representation that shows all loops,
;;;      array indices, arithmetic operations, and potential reductions.
;;;    - This blueprint is then suitable for final code generation (e.g., translating into C code).
;;; In other words, `lower-schedule-item` turns a high-level fused kernel node into a fully described,
;;; low-level intermediate form (the blueprint) that can be compiled into efficient machine code or C code.

;;; Here we lower `item1` using the `lower-schedule-item` function. The `base-graph` is given by `(avm-graph graph)`,
;;; and `scheduled-graph` is the optimized graph after scheduling. This will mutate `item1` so that it
;;; now has a `:blueprint` attribute containing the lowered form.
(defparameter lowered-item1 (lower-schedule-item item1 (avm-graph graph) scheduled-graph))

;;; After this, `lowered-item1` (which should be the same as `item1` but now lowered)
;;; will hold the blueprint. Printing it might show a node with `:expr` indicating
;;; a fully lowered representation of the computation (e.g., `sin(sin(x + pi/2))`).
(print lowered-item1)
;;; Processing (PRINT LOWERED-ITEM1)
;;; (<Node[JIT] EXPR(NID1505) : val_6* <- (1.5707964, val_0) where :expr=#<EXPR sin(sin((1.0+1.5707964)))> :_type_relay=#<INFERRED-TYPE ((1)) <- (NIL(1))>>)

;;; The :expr attribute describes the blueprint code represented as a low-level computation

;;; print-blueprint allows us to see the blueprinted code before the code-gen step
(print (print-blueprint (getattr item1 :blueprint) nil))
;;; Blueprint code generated by print-blueprint
;;; {
;;;   val_6[0] = sin(sin((1.0+1.5707964)));
;;; }

;;; Now after all these intermediate steps, it's time to generate the actual kernel!
;;; The function below makes sure parameters and other necessary variables are properly initialized and declared.
(schedule-item-write-define-global item1)

;;; Create a renderer instance
(defparameter renderer (make-instance 'CStyle-Renderer))

(print renderer)

;;; render-kernel is responsible for taking the previously generated C code for all jitable items, compiling it into a shared library or native code, and then making the resulting functions callable from Lisp.
(let ((c-kernel (%render-kernel renderer item1)))
  (format t "Generated C code:~%~a~%" c-kernel)
  (setf (getattr item1 :rendered-object) c-kernel))

;;; We can visualize the generated kernel for our high-level function we defined previously, called sincos!
;;; Generated C code:
;;; void fused_sin_sin_add_load_load964(float* val_6);
;;; void fused_sin_sin_add_load_load964(float* val_6) {
;;;   val_6[0] = sin(sin((1.0+1.5707964)));
;;; }

;;; %compile-kernel transforms the high-level Lisp IR (intermediate representation) into optimized native code, loads that code, and provides a way for Lisp to call it directly—completing the JIT compilation pipeline

;;; We make a copy of the original graph without modifications
(defparameter original-graph (copy-graph (avm-graph graph)))

;;; Replace the compiled kernels in the graph
(setf (avm-graph graph) (schedule-graph->avm-graph (avm-graph graph) scheduled-graph))

;;; The original graph contains the high-level operations still.
(print original-graph)
;;; Notice how the original graph has the high-level nodes still!
;;; Graph[seen=NIL, outputs=NIL] {
;;; <ALLOCATE : val_0 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE ((1)) <- NIL> :_read_views=(NIL NIL) :_output_type=NIL>
;;; <Node[BUFFER] LOAD(NID492) : val_1* <- (val_0) where :value=1.0 :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL) :declare-type=(T)>
;;; <Node[BUFFER] ALLOCATE(NID499) : val_2* <- () where :nrank=0 :dtype=FLOAT32 :_type_relay=#<INFERRED-TYPE (NIL) <- NIL>>
;;; <Node[BUFFER] LOAD(NID501) : val_3* <- (val_2) where :value=1.5707964 :_type_relay=#<INFERRED-TYPE (NIL) <- (NIL)> :_read_views=(NIL) :declare-type=(T)>
;;; <Node[BINARYOPS] ADD(NID503) : val_4* <- (val_1, 1.5707964) where :_type_relay=#<INFERRED-TYPE ((1)) <- (NIL (1))> :_read_views=(NIL NIL)>
;;; <Node[UNARYOPS] SIN(NID505) : val_5* <- (val_4) where :_type_relay=#<INFERRED-TYPE ((1)) <- (NIL (1))> :_read_views=(NIL) :declare-type=(T)>
;;; <Node[UNARYOPS] SIN(NID507) : val_6* <- (val_5) where :_type_relay=#<INFERRED-TYPE ((1)) <- (NIL (1))> :_read_views=(NIL)>
;;; <Node[SPECIAL/VM] PAUSE/BACKWARD(NID520) : val_7* <- (val_6) where :_type_relay=#<INFERRED-TYPE ((1)) <- ((1))> :_read_views=(NIL)>
;;; }

;;; The overwritten graph has the JIT kernel we generated previously.
;;; The schedule-graph->avm-graph function integrates the compiled kernels.

(print graph)
;;; Processing (PRINT GRAPH)
;;; #S(AVM
;;;    :GRAPH 
;;;    Graph[seen=NIL, outputs=NIL] {
;;;      <ALLOCATE : val_0 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_type_relay=NIL :_read_views=NIL :_output_type=NIL>
;;;      <ALLOCATE : val_6_1 <- (shape=(1), stride=(1)) where :nrank=1 :dtype=FLOAT32 :_read_views=NIL :_output_type=NIL>
;;;      <Node[JIT] JIT_KERNEL(NID1335) : val_6* <- (val_6_1) where :output-buffer-n=1 :kernel-info=<CLANG[FUSED_SIN_SIN_ADD_LOAD_LOAD840]> :dtypes=(FLOAT32)>
;;;      <Node[BUFFER] ALLOCATE(NID499) : val_2* <- () where :nrank=0 :dtype=FLOAT32>
;;;      <Node[SPECIAL/VM] PAUSE/BACKWARD(NID520) : val_7* <- (val_6)>
;;;    }
;;;    :NAME :MAIN476
;;;    :FW-OUTPUTS (|val_7|)
;;;    :BW-OUTPUTS NIL
;;;    :ID2TENSOR #<HASH-TABLE :TEST EQL :COUNT 1 {100157D2B3}>
;;;    :TAPE-LENGTH 8
;;;    :PC 8
;;;    :VARIABLES #<HASH-TABLE :TEST EQL :COUNT 10 {100157D353}>
;;;    :DUMPED NIL) 

(avm-reset graph) ;; Ensure the graph is reset

;;; Finally, we can run the lower-level graph with the replaced nodes!

;;; Run the computation using %run
(print (%run graph (cons 'val_0 1.0)))
;;; Expected Output:
;;; The result of sin(sin(1.0 + π/2)) is printed, computed using the optimized, compiled kernel.

;;; 4. Execution Explanation

;;; The `%run` method orchestrates the execution of the compiled kernel within the AVM context. Here's how it works:

;;; (defmethod %run ((avm caten/avm:AVM) &rest params)
;;;   (let ((*device* (ctx:getenv :AVM))
;;;         (params (loop for (key . val) in params collect (cons key (if (tensor-p val) (tensor-buffer val) val)))))
;;;     (vm/set-params avm params)
;;;     (vm/forward avm)
;;;     (avm/sync-tensors avm)
;;;     (flet ((ap (x) (gethash x (avm-variables avm))))
;;;       (apply #'values (map 'list #'ap (avm-fw-outputs avm))))))
;;;
;;; 5. Final Summary

;;; In this end-to-end tutorial, we've explored the complete pipeline of defining, compiling, and executing a computation graph using Caten's AVM. Here's what we've covered:

;;; 1. **High-Level Definition:**
;;;    - Defined a `SinCos` function that computes `sin(cos(x))` using a high-level class structure similar to modules in deep learning frameworks.

;;; 2. **Graph Construction:**
;;;    - Created a tensor and instantiated the `SinCos` function, resulting in a lazy tensor representation.
;;;    - Generated an intermediate representation (IR) graph using the `caten` function, which details each computation step.

;;; 3. **Type Inference and Rewriting:**
;;;    - Ran type inference to propagate type and shape information through the graph.
;;;    - Applied rewriting rules to rename variables and prepare the graph for optimization and code generation.

;;; 4. **Graph Scheduling and Optimization:**
;;;    - Scheduled the graph to identify and fuse operations into optimized kernels.
;;;    - Generated a FastGraph that consolidates operations like `ADD` and `SIN` into a single `JIT_KERNEL` node for efficiency.

;;; 5. **Kernel Lowering and Compilation:**
;;;    - Lowered the scheduled kernel into a blueprint, a low-level representation suitable for code generation.
;;;    - Rendered the blueprint into C code and compiled it into a native kernel using `%render-kernel` and `%compile-kernel`.
;;;    - Integrated the compiled kernel back into the AVM graph using `schedule-graph->avm-graph`, replacing high-level nodes with the optimized kernel.

;;; 6. **Execution with AVM:**
;;;    - Reset the AVM to ensure a clean execution state.
;;;    - Executed the computation graph using the `%run` method, which set input parameters, ran the forward pass, synchronized tensors, and retrieved the results.
;;;    - Leveraged the compiled kernel for efficient computation, resulting in the final output of `sin(sin(1.0 + π/2))`.

