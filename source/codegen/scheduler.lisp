(defpackage #:caten/codegen/scheduler
  (:documentation "
`caten/codegen/scheduler` is responsible for partitioning a (huge amount of!) computation graph into several smaller subgraphs with the same iteration space.

`graph-schedule` as en entry point, it receives a graph of `caten/aasm` created by running `caten/apis/iseq.lisp`, and returns a graph of `Schedule-Item`.
One Schedule-Item corresponds to one kernel in GPU. Therefore, in general, the more computation grouped in the same group, the better, in term of the memory-locality. Loops may be distributed elsewhere, but never fused except for `recursive-create-group`.")
  (:use :cl :caten/air :caten/codegen/expr)
  (:import-from :caten/aasm #:JITAble)
  (:import-from
   #:caten/air
   #:defnode
   #:Node
   #:node-type
   #:node-attr
   #:FastGraph
   #:graph-outputs
   #:id->value
   #:id->users
   #:copy-node)
  (:import-from
   #:caten/avm
   #:Buffer
   #:copy-buffer
   #:buffer-dtype
   #:buffer-shape
   #:buffer-stride
   #:buffer-views
   #:buffer-nrank
   #:buffer-inferred-permute)
  (:import-from
   #:caten/codegen/shape-inference
   #:mergeable-view-p
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters
   #:buffer-merge-dims
   #:iteration-space
   #:iteration-space-shape
   #:iteration-space-strides
   #:iteration-space-views
   #:iteration-space-procedure)
  (:import-from
   #:caten/codegen/helpers
   #:range
   #:permute-list
   #:nodes-depends-on
   #:nodes-write-to
   #:ensure-string-as-compilable)
  (:import-from
   #:caten/codegen/rewriting-rules
   :nodes-apply-static-gensym)
  (:export
   #:Group
   #:make-group
   #:graph-schedule
   #:*function-name-maxlen*
   #:group->schedule))

(in-package #:caten/codegen/scheduler)

(defparameter *function-name-maxlen* 64 "Restricts the maximum length of the function name autogenerated by the compiler.")

(defnode (:GRAPH :Schedule-Item) ()
         "
Schedule-Item is an intermidate object to represent a one kernel in GPU.

```
f = cache_name or name
write_ids = f(*[storage_id_dst], *[dynamic_shape], *[inputs])
                      ^ can be modified by the memory-planner
```

It has a unique `name`, and `cache-name`. If `cache-name` was assigned, the compiler will fail to compile this schedule-item and reuse the kernel named `cache-name` instead.

In order to lowering the computation graph as the foreign language, `items` must be consisted of JITAble operations (except for special irs and :allocate). If it qualifies, `jitable` is set to T.

Otherwise, the scheduled items are relocated to the compiled avm directly. Specifially, if the item was :ALLOCATE, :allocated-p is set to T.

- blueprint[list] is a lowered schedule-item
- polyhedral[list] is a Polyhedral IR obtained by lowering blueprint
- auto-schedule-p[list] is set to T if it is worth to run auto-scheduler. (If there is a symbolic incremental, loop is not an affine and cannot run isl scheduler)
- items[list] are the scheduled items
- items-to-cache[list] are the copies of items but having the unique read/write. It is used to determine the equivalence of the two schedule-items.
- rank[fixnum] is the highest rank of the iteration space.
- storage-id-src[list] is the list of the storage-id of the source buffer (optimized by running memory-planner)
- storage-id-dst[list] is the list of the storage-id of the destination buffer (optimized by running memory-planner)
"
         :slots
         ((blueprint :type list :initform nil)
          (polyhedral)
          (jitable :type boolean)
          (allocate-p :type boolean)
          (auto-schedule-p :type boolean)
          (name :type symbol) (cache-name :type symbol)
          (items :type list) (items-to-cache :type list)
          (rank :type fixnum) (reduce-dims :type list)
          (read-types :type list) (write-types :type list)
          (reference-counters :type list)
          (storage-id-src :type list)
          (storage-id-dst :type list)
          (dynamic-shapes :type list)
          (rendered-object :type string)
          (compiled-object :type list)))

(defmethod print-node (node (id (eql :Schedule-Item)))
  (flet ((r (x y)
           (apply
            #'concatenate
            'string
            (butlast
             (loop for x1 in x
                   for nth upfrom 0
                   for y1 = (nth nth y)
                   if (or (eql x1 y1) (null y1))
                     append (list (format nil "~a" x1) ", ")
                   else
                     append (list (format nil "~a[~a]" x1 y1) ", "))))))
    (format nil "{ ~a } : [ ~a <- ~a where lowered-p=~a ~a]"
            (if (getattr node :allocate-p)
                "Allocate"
                (if (getattr node :jitable)
                    " KERNEL "
                    "  VMOP  "))
            (r (node-writes node) (getattr node :storage-id-dst))
            (if (getattr node :allocate-p)
                (subseq (node-reads (car (getattr node :items))) 0 (getattr (car (getattr node :items)) :nrank))
                (r (node-reads node) (getattr node :storage-id-src)))
            (if (getattr node :blueprint)
                "t" "nil")
            (if (getattr node :allocate-p)
                ""
                (if (getattr node :cache-name)
                    (format nil ":cache-name=~a :name=~a" (getattr node :cache-name) (getattr node :name))
                    (format nil ":name=~a" (getattr node :name)))))))
;; ~~ Scheduler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Group
  (key (gensym) :type symbol)
  (reduce-dims nil :type list)
  (items nil :type list))

(defmethod verify-group ((group Group))
  (when (find :Allocate (group-items group) :key #'node-type)
    (assert (= (length (group-items group)) 1) () "Allocate should be scheduled standalone")))

(defun pname (name)
  (cl-ppcre:regex-replace-all
   "PAUSE/"
   (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "GRAPH/" (princ-to-string name) "") "_")
   ""))

(defmethod make-unique-schedule-name ((group Group))
  (let ((names-func)
        (module-names)
        (seen))
    (dolist (item (group-items group))
      (if (and (typep (node-attr item) 'JITAble) (car (getattr item :_lowering_history))) ;; JITAble nodes have a lowering history
          (multiple-value-bind (name id) (values (caar (getattr item :_lowering_history)) (cdar (getattr item :_lowering_history)))
            ;; history = (module_name, module_id)
            (when (null (find id seen))
              (push id seen)
              (push (pname name) module-names)))
          (push (pname (node-type item)) names-func)))
    (let ((key
            (with-output-to-string (out)
              (princ "FUSED" out)
              (dolist (n (or module-names names-func))
                (format out "_~a" (ensure-string-as-compilable (princ-to-string n)))))))
      (when (> (length key) *function-name-maxlen*)
        (setf key (subseq key 0 *function-name-maxlen*)))
      (gensym key))))

(defun items-write-to (items graph)
  (let ((result (nodes-write-to items)) (wrap (map 'list #'node-id items)))
    (loop for item in items do
      (loop for write in (node-writes item)
            for users = (id->users graph write)
            if (some #'(lambda (x) (null (find (node-id x) wrap))) users) ;; user is read outside of the items
              do (push write result)))
    (remove-duplicates result)))

(defmethod group->schedule ((group Group) (base-graph Graph))
  (let ((reads (nodes-depends-on (group-items group)))
        (writes (items-write-to (group-items group) base-graph))
        (allocate-p (find :Allocate (group-items group) :key #'node-type))
        (no-symbolic-incremental-p t)
        (full-scalar-p t) (rank 0))
    ;; Ensure there's no symbolic incremental for the auto scheduler.
    (dolist (node (group-items group))
      (dolist (r (append (relay-reads (read-type-relay node)) (relay-writes (read-type-relay node))))
        (when r
          (when (> (buffer-nrank r) 0)
            (setf full-scalar-p nil))
          (setf rank (max rank (buffer-nrank r)))
          (dolist (v (buffer-views r))
            (when (and v (third v) (symbolp (third v))) ;; v=(upfrom below by broadcast_p)
              (setf no-symbolic-incremental-p nil))))))
    (make-node :GRAPH :Schedule-Item writes reads :name (make-unique-schedule-name group)
               :jitable (and (every #'jitable-p (group-items group)) (null full-scalar-p))
               :allocate-p (when allocate-p t)
               :auto-schedule-p (and no-symbolic-incremental-p (null full-scalar-p))
               :storage-id-dst writes
               :storage-id-src reads
               :reference-counters
               (map
                'list
                #'(lambda (x)
                    (+
                     (if (find x (graph-outputs base-graph)) 1 0)
                     (length (id->users base-graph x))))
                (append writes reads))
               :rank rank
               :reduce-dims (group-reduce-dims group)
               :items (group-items group)
               :items-to-cache (nodes-apply-static-gensym (map 'list #'copy-node (group-items group))))))

(defmethod merge-schedule-items ((si1 Node) (si2 Node) (base-graph Graph))
  (assert (eql (node-type si1) :Schedule-Item))
  (assert (eql (node-type si2) :Schedule-Item))
  (assert (= (getattr si1 :rank) (getattr si2 :rank)))
  (assert (or (null (getattr si1 :reduce-dims)) (null (getattr si2 :reduce-dims)) (equal (getattr si1 :reduce-dims) (getattr si2 :reduce-dims))))
  (group->schedule
   (make-group :items (append (getattr si1 :items) (getattr si2 :items))
               :reduce-dims (or (getattr si1 :reduce-dims) (getattr si2 :reduce-dims)))
   base-graph))

(defmethod group-get-type ((group Group))
  (let* ((last (nodes-write-to (group-items group)))
         (node (when last (find (car last) (group-items group) :key #'node-writes :test #'find))))
    (when node
      (car (relay-writes (read-type-relay node))))))

(defmethod jitable-p ((node Node))
  (and
   (null (find (node-type node) `(:ALLOCATE :PAUSE/BACKWARD)))
   (typep (node-attr node) 'JITAble)))

(defmethod node-reduce-axes ((node Node))
  (when (getattr node :reduction :allow-undefined t)
    (let ((write-buffer (car (relay-writes (read-type-relay node)))))
      (let ((out
              (loop for v in (buffer-views write-buffer)
                    for s in (buffer-shape write-buffer)
                    if (fourth v)
                      collect s
                    else
                      collect nil)))
        ;; Returns uncollapsed rank list
        (when (some #'identity out) out)))))

(defmethod group-items-st-rewriter ((group Group) f mask)
  (dolist (item (group-items group))
    (when (eql (node-type item) :INDEX-COMPONENTS) ;; (cdr (node-reads index-components)) also represents for the stride
      (setf (node-reads item)
            (append
             (list (car (node-reads item)))
             (loop with s = (cdr (node-reads item))
                   for m in mask
                   if m collect 1 else collect (pop s)))))
    (loop for typ in (relay-reads (read-type-relay item))
          for nth upfrom 0
          unless (or (null typ) (= 0 (buffer-nrank typ)))
            do (setf (nth nth (relay-reads (read-type-relay item))) (or (funcall f typ) typ)))
    (loop for typ in (relay-writes (read-type-relay item))
          for nth upfrom 0
          unless (or (null typ) (= 0 (buffer-nrank typ)))
            do (setf (nth nth (relay-writes (read-type-relay item))) (or (funcall f typ) typ)))))

(defmethod group-rank ((group Group))
  (let ((buff (group-get-type group)))
    (when buff (buffer-nrank buff))))

(defun apply-view-fusor (tgt-rank mask group)
  ;; T = broadcasted, NIL = old axes2
  (group-items-st-rewriter
   group
   #'(lambda (typ)
       (when (= (buffer-nrank typ) tgt-rank)
         (let ((typ (copy-buffer typ)))
           (let ((shp (copy-list (buffer-shape typ)))
                 (str (copy-list (buffer-stride typ)))
                 (views (copy-list (buffer-views typ))))
             (setf (buffer-shape typ)
                   (loop for b in mask
                         if b collect 1 else collect (or (pop shp) 1))
                   (buffer-stride typ)
                   (loop for b in mask
                         if b collect 0 else collect (or (pop str) 1))
                   (buffer-views typ)
                   (loop for b in mask
                         if b collect `(0 1 1 t) else collect (pop views))
                   (buffer-nrank typ) (length (buffer-shape typ)))
             ;; Consumed all masks?
             (assert (= (buffer-nrank typ) (length mask)))
             typ))))
   mask))

(defmethod apply-index-component-fusion ((group Group) permute)
  "Permutes the strides of :INDEX-COMPONENTS in the group in the group"
  (dolist (item (group-items group))
    (when (and (eql (node-type item) :INDEX-COMPONENTS) (= (length permute) (length (cdr (node-reads item)))))
      (setf (cdr (node-reads item)) (permute-list permute (cdr (node-reads item)))))))

(defun broadcastable-p (prev new)
  (let ((prev-shape (copy-list (buffer-shape prev)))
        (new-shape  (copy-list (buffer-shape new))))
    (let ((p (loop for p in prev-shape if (not (eql p 1)) collect p))
          (n (loop for n in new-shape if (not (eql n 1)) collect n)))
      (equal p n))))

(defun buffer-mergeable-p (g b1 b2)
  (flet ((lazy-eq (a b)
           (or (eql a 1) (eql b 1) (eql a b)
               (and (symbolp a) (symbolp b) (expr-scalar-equivalent-p (expr-from-graph a g) (expr-from-graph b g))))))
    (every #'lazy-eq (buffer-shape b1) (buffer-shape b2))))

(defun buffer-complex-out-fusable-p (g b1 b2 mask)
  "An extra mergeable condition not to introduce an extra dim after reduction is performed.
g represents for Graph, b1 for the self buffer, b2 for the parent buffer, mask for the reduced dims."
  (declare (type Graph g) (type buffer b1 b2) (type list mask))
  (assert (= (buffer-nrank b1) (buffer-nrank b2)))
  (flet ((lazy-eq (a b nth m)
           ;; b = a buffer belongs to grouped schedule.
           ;; a = a buffer newly introducing.
           (let ((a-view (nth nth (buffer-views b1)))
                 (b-view (nth nth (buffer-views b2))))
             (if m
                 ;; Reduced dims ->
                 (flet ((ok (size view)
                          (or (eql size 1) (fourth view) (eql m 1))))
                   (and
                    (ok a a-view)
                    (ok b b-view)))
                 ;; Non-reduced dims -> mergeable as long as they have the same shape, or broadcasted.
                 (or (eql a 1) (eql b 1) (eql a b)
                     (and (symbolp a) (symbolp b) (expr-scalar-equivalent-p (expr-from-graph a g) (expr-from-graph b g))))))))
    (every #'lazy-eq (buffer-shape b1) (buffer-shape b2) (range 0 (buffer-nrank b1)) mask)))

(defun group-assert-rank (group r1 r2 view &aux (rank (max r1 r2)))
  (loop for item in (group-items group)
        do (loop for typ in (append (relay-reads (read-type-relay item)) (relay-writes (read-type-relay item)))
                 for nth upfrom 0
                 unless (or (null typ) (= 0 (buffer-nrank typ)))
                   do (assert (= rank (buffer-nrank typ)) ()
                              "Rank mismatch: (expected from ~a -> ~a)~%view=~a~%buffer:~%~a~%group~%~a"
                              (min r1 r2) rank view typ group))))

(defmethod identify-view-type ((view Node))
  (assert (eql :VIEW (node-type view)))
  (when (some #'identity (getattr view :broadcast)) (return-from identify-view-type :broadcast))
  (when (not (equal (getattr view :permute) (range 0 (getattr view :nrank))))
    (return-from identify-view-type :permute))
  (flet ((shrink-p (size view)
           (assert (= (length view) 4) () "not a view")
           (multiple-value-bind (from to by broadcast) (apply #'values view)
             (assert (null broadcast))
             (or
              (not (eql from 0))
              (not (eql to size))
              (not (eql by 1))))))
    (let* ((base-buffer (car (relay-reads (read-type-relay view))))
           (views (buffer-views (car (relay-writes (read-type-relay view))))))
      (when (and
             (= (buffer-nrank base-buffer) (getattr view :nrank))
             (some #'shrink-p (buffer-shape base-buffer) views))
        (return-from identify-view-type :shrink)))
    :reshape))

(defmethod group-merge-p ((self Group) (graph Graph) (node Node) (parent-group Group) nth)
  (symbol-macrolet ((->ok
                      (progn
                        (setf (group-reduce-dims self) (or (group-reduce-dims self) (group-reduce-dims parent-group)))
                        (return-from group-merge-p t)))
                    (->ng (return-from group-merge-p nil)))
    (when (and (group-reduce-dims self) (group-reduce-dims parent-group))
      ;; Both groups are reduced?
      (when (not (equal (group-reduce-dims self) (group-reduce-dims parent-group)))
        ;; Reduced at the same rank?
        ->ng))
    (let* ((read (nth nth (node-reads node)))
           (read-node (id->value graph read))
           (read-view (car (nth nth (getattr node :_read_views))))
           (read-type (group-get-type parent-group)))
      (assert (<= (length (nth nth (getattr node :_read_views))) 1))
      ;; Relations between group and parent-group:
      ;; ```
      ;; group=parent | X[write_type]{write_iter} = f(...)
      ;; group=self   | ... = f(..., X[read_type]{read_iter})
      ;; ```
      (when (or (not (jitable-p node)) (not (jitable-p read-node)))->ng)
      ;; ~~ merge views ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ;; :shrink is not mergeable
      (when (and read-view (eql (identify-view-type read-view) :shrink))
        ;; [TODO] Add a mask like:
        ;; _gid0 >= 2 && _gid1 >= 2 ? move1 : move2
        ->ng)
      (let ((r1 (group-rank self))
            (r2 (group-rank parent-group)))
        ;; r2 -> r1
        (cond
          ((or (= r1 0) (= r2 0))->ok)
          ((= r1 r2)
           (when (group-reduce-dims parent-group)
             ;; Complex-Out-Fusable: After the reduction is performed in the parent group, only the buffers which does not introduce new axis, are allowed to be merged.
             ;; does not introduce new axis = the total element size is the same as the reducing parent group.
             (if (buffer-complex-out-fusable-p graph (group-get-type self) (group-get-type parent-group) (group-reduce-dims parent-group))
                 ->ok
                 ->ng))
           (if (buffer-mergeable-p graph (group-get-type parent-group) (group-get-type self))
               ->ok
               ->ng))
          (T
           (let ((self-type (group-get-type self))
                 (c (< r1 r2)))
             (when (null self-type)->ng)
             (if (broadcastable-p read-type self-type)
                 (let ((mask (map 'list #'(lambda (x) (eql x 1)) (buffer-shape (if c read-type self-type)))))
                   (assert (some #'identity mask))
                   (apply-view-fusor (min r1 r2) mask self)
                   (apply-view-fusor (min r1 r2) mask parent-group)
                   (when (and read-view (getattr read-view :permute))
                     (apply-index-component-fusion parent-group (getattr read-view :permute)))
                   (group-assert-rank self r1 r2 read-view)
                   (group-assert-rank parent-group r1 r2 read-view)
                   ->ok)
                 (if (and read-view (some #'identity (getattr read-view :broadcast)))
                     (let ((mask (getattr read-view :broadcast)))
                       (when (not (= (length mask) (max r1 r2)))
                         (setf mask (map 'list #'fourth (buffer-views (if c read-type self-type)))))
                       (when (not (= (length mask) (max r1 r2)))->ng)
                       (apply-view-fusor (min r1 r2) mask self)
                       (apply-view-fusor (min r1 r2) mask parent-group)
                       (when (and read-view (getattr read-view :permute))
                         (apply-index-component-fusion parent-group (getattr read-view :permute)))
                       (group-assert-rank self r1 r2 read-view)
                       (group-assert-rank parent-group r1 r2 read-view)
                       ->ok)
                     ->ng)))))))))

(defmethod merge-groups ((self Group) parents mergeable-list)
  (let* ((p (loop for m in mergeable-list for p in parents
                  if m collect p))
         (ranks (map 'list #'group-rank p))
         (srank  (group-rank self)))
    (loop for r in ranks
          for group in p
          for eql-p = (or (eql 0 r) (eql 0 srank) (= r srank))
          unless eql-p do
            (assert (< r srank))
            (let* ((typ1 (group-get-type self))
                   (m (map 'list #'fourth (buffer-views typ1))))
              (assert (some #'identity m))
              (apply-view-fusor r m group)
              (group-assert-rank group srank srank nil))))
  (loop for m in mergeable-list
        for p in parents
        if m do
          (assert (or (null (group-reduce-dims p)) (null (group-reduce-dims self)) (equal (group-reduce-dims self) (group-reduce-dims p)))
                  ()
                  "Reduce dims = ~a ~a" (group-reduce-dims p) (group-reduce-dims self))
          (setf (group-items self) (append (group-items p) (group-items self))
                (group-reduce-dims self) (or (group-reduce-dims self) (group-reduce-dims p))))
  self)

(defun recursive-create-groups (id graph &key (seen))
  (declare (type symbol id) (type graph graph) (type hash-table seen))
  (when (gethash id seen) (return-from recursive-create-groups))
  (setf (gethash id seen) t)
  (let* ((node (id->value graph id))
         (self
           (make-group
            :items (list node)
            :reduce-dims (node-reduce-axes node)))
         (parents
           (reverse (map 'list #'(lambda (x) (and (symbolp x) (recursive-create-groups x graph :seen seen))) (reverse (node-reads node))))))
    (declare (type node node) (type list parents))
    ;; Consider this structured graph:
    ;; parents[0] parents[1] parents[2] ...
    ;;        \      |      /
    ;;              self
    ;;               |
    ;; => The function returns this flattend list
    ;; (list               (list
    ;;   parents[0]          parent[2]
    ;;   parents[1]            ...
    ;;   parents[2]    =>    group(items=self+parent[0]+parent[1]))
    ;;     ...
    ;;   self)
    ;;  (No fuse)        (When parent[0] and parent[1] are fusable)
    (let ((mergeable-p-list
            (loop for parent in parents
                  for parent-return = (car parent)
                  for nth upfrom 0
                  if parent-return
                    collect (group-merge-p self graph node parent-return nth)
                  else
                   collect nil)))
      (assert (= (length mergeable-p-list) (length parents)))
      (append
       (list (merge-groups self (map 'list #'car parents) mergeable-p-list))
       (loop for p in parents
             for m in mergeable-p-list
             if m ;; mergeable
               append (cdr p)
             else ;; unmergeable
             append p)))))
;; ~~~~~~ More Fusion Rules ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Rewrite them as a pattern matcher.
(defun apply-schedule-item-fusor (f schedule-graph base-graph &aux (seen) (changed-p t))
  (declare (optimize (speed 3))
           (type function f)
           (type graph schedule-graph base-graph)
           (type list seen))
  (labels ((parent-groups (self)
             (assert (node-p self))
             (loop for r in (node-reads self)
                   for val = (and (symbolp r) (id->value schedule-graph r))
                   ;; Only :jitable scheduleitems are merged
                   if val collect val))
           (can-split-p (id)
             (<= (length (the list (id->users schedule-graph id))) 1))
           (explore (id)
             (let* ((self (id->value schedule-graph id))
                    (_ (when (or (null self) (find (node-id self) seen)) (return-from explore)))
                    (candidates (parent-groups self))
                    (self-mergeable-p (getattr self :jitable)))
               (declare (ignore _))
               (push (node-id self) seen)
               (loop for parent in candidates
                     if (and self-mergeable-p parent
                             (getattr parent :jitable)
                             (every #'can-split-p (node-writes parent))
                             (funcall f self parent))
                       do (let ((merged (merge-schedule-items self parent base-graph)))
                            (setf changed-p t)
                            (insert-nodes schedule-graph (list merged))
                            (dolist (w (node-writes parent))
                              (remnode schedule-graph w))))
               (mapc #'explore (node-reads self)))))
    (loop while changed-p do
      (setf changed-p nil seen nil)
      (mapc #'explore (graph-outputs schedule-graph)))))
      
(defun apply-reduce+move-fusion (schedule-graph base-graph)
  "Applies the post-loop-fusion to eliminate MOVE after the reduction.
```
     Group1
       |
      SELF
```
Consider the case where group1 and self fusion was rejected by the group-reduce-dims rule, group1 is a reduction, and `MOVE` after the reduction is merged to `self`. For example:
```
[Group1]
{
  int _acc_0 = 0;
  for (...)
    _acc_0 += ...;
  // Expected element wise operation here! otherwise we have to mutate _acc_0 as an array...
}
[SELF]
{
  for (...)
   out[...] = f(_acc_0);
}
```
This function enumerates all such pairs and allows all reduction operations to have a `MOVE` node after the reduction by serializing the loop. (This scheduling pattern can be observed by running an operation with multiple reductions in a single kernel, such as `!argmax`, or `RMSNorm`, etc.)

To put it bluntly, this function explores all `reduced-but-not-stored` pairs, and merges two schedule items who shares `reduced-but-not-stored`.

If this interrupts the parallelism, AutoScheduler should distribute them and create a shared buffer."
  (declare (type FastGraph schedule-graph))
  (flet ((reduce-w/o-store (self)
           (loop for id in (node-writes self) ;; only the output of the kernel matters
                 for item = (find id (getattr self :items) :key #'node-writes :test #'find)
                 if (getattr item :reduction :allow-undefined t)
                   collect item)))
    (apply-schedule-item-fusor
     #'(lambda (self parent) (declare (ignore self)) (reduce-w/o-store parent))
     schedule-graph
     base-graph)))

(defun apply-serialize-reduction (schedule-graph base-graph)
  (flet ((is-tensor (buffer)
           (not (every #'(lambda (x) (eql x 1)) (buffer-shape buffer))))
         (depend-dims-p (items rank &aux (common-views (make-list rank)))
           (loop for item in items do
             (loop for rt in (relay-reads (read-type-relay item))
                   if (some #'identity (buffer-views rt)) do
                     (loop for nth upfrom 0
                           for view in (buffer-views rt) do
                             (setf (nth nth common-views) (or (nth nth common-views) (fourth view))))))
           (and (every #'null (butlast common-views))))
         (no-index-components-p (si)
           (null (find :INDEX-COMPONENTS (getattr si :items) :key #'node-type))))
    (apply-schedule-item-fusor
     #'(lambda (self parent)
         (let ((self-type (group-get-type (make-group :items (getattr self :items))))
               (parent-type (group-get-type (make-group :items (getattr parent :items)))))
           (and
            (no-index-components-p self) ;; [TODO] Merge Index Components
            (no-index-components-p parent)
            (= (getattr self :rank) (getattr parent :rank)) ;; Make sure not extra loop is introduced
            (buffer-mergeable-p base-graph self-type parent-type)
            (is-tensor self-type) (is-tensor parent-type)
            (or
             (null (getattr self :reduce-dims))
             (null (getattr parent :reduce-dims))
             (and
              (equal (getattr self :reduce-dims) (getattr parent :reduce-dims))
              (depend-dims-p (getattr self :items) (getattr self :rank))
              (depend-dims-p (getattr self :items) (getattr parent :rank)))))))
     schedule-graph
     base-graph)))

(defun apply-move-after-reduction (schedule-graph)
  (labels ((%newtype (buffer)
             (caten/avm:make-buffer
              (buffer-nrank buffer)
              (loop for s in (buffer-shape buffer)
                    for nth upfrom 0
                    for v = (nth nth (buffer-views buffer))
                    if (and (listp v) (fourth v)) ;; broadcasted
                      collect 1
                    else
                      collect s)
              (buffer-stride buffer)
              (buffer-dtype buffer)
              (loop for s in (buffer-shape buffer)
                    for nth upfrom 0
                    for v = (nth nth (buffer-views buffer))
                    if (and (listp v) (fourth v))
                      collect `(0 1 1 t)
                    else
                      collect v)))
           (%jstore (w a b base-type)
             (make-node :Buffer :MOVE (list w) (list a b)
                        :_type_relay
                        (caten/codegen/shape-inference:make-inferred-type
                         (list (%newtype base-type))
                         (list (%newtype base-type) (%newtype base-type)))))
           (acc (id) (intern (format nil "~(~a~)_acc" id)))
           (r (si &aux (g (apply #'make-graph (getattr si :items))))
             (dolist (node (graph-nodes g))
               ;; reduced but no users in the group: This is now allowed. Adding :MOVE
               (when (and (getattr node :reduction :allow-undefined t)
                          (null (id->users g (car (node-writes node))))) ;; no users in a group
                 (let ((base-id (car (node-reads node)))
                       (write-id (car (node-writes node)))
                       (acc-id (acc (car (node-reads node)))))
                   (setf (node-writes node) (list acc-id))
                   ;; val_3 = val_3_acc
                   ;; ```
                   ;; float val_3 = 0.0;               float val_3 = 0.0;
                   ;; for (...) val_4 = val_3+1.0  =>  for (...) val_3_acc = val_3+1.0;
                   ;; ```                              val_4[0] = val_3_acc;
                   (push
                    (%jstore write-id base-id acc-id (car (relay-writes (read-type-relay node))))
                    (getattr si :items)))))))
    (mapc #'r (graph-nodes schedule-graph))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric graph-schedule (graph) (:documentation "Splits a given graph into small subgraphs called Schedule-Item. It always returns `FastGraph`."))

(defmethod graph-schedule ((graph Graph))
  (let* ((seen (make-hash-table))
         (groups (apply #'append (map 'list #'(lambda (x) (recursive-create-groups x graph :seen seen)) (graph-outputs graph)))))
    (mapc #'verify-group groups)
    (when (>= (ctx:getenv :JIT_DEBUG) 4)
      (format t "[graph-schedule] Prescheduled ~a groups:~%" (length groups))
      (dolist (g groups)
        (when (not (eql (node-type (car (group-items g))) :Allocate))
          (print g)))
      (fresh-line))
    (let ((schedule (apply #'make-graph (map 'list #'(lambda (x) (group->schedule x graph)) groups))))
      (setf (graph-outputs schedule) (graph-outputs graph))
      (setf schedule (->fast-graph schedule))
      ;; ~~ Rewriting Rules + Post Fusion ~~~~~
      (apply-reduce+move-fusion schedule graph)
      (apply-serialize-reduction schedule graph) ;; (TODO: Only execute when MAXIMIZE_MEMORY_LOCALITY=1?)
      (apply-move-after-reduction schedule)
      ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      (when (>= (ctx:getenv :JIT_DEBUG) 3)
        (format t "[graph-schedule] Schedule Graph:~%~a~%" schedule))
      schedule)))
