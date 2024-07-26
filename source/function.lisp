(in-package :caten)

(defstruct (IntermidateTape))
;; A List of Func/Module
(defclass Func ()
  ((variables :initarg :variables :initform nil :accessor func-variables)))

(defgeneric lower (op &rest inputs)
  (:documentation "Lowers the Func into a list of `caten/air:node`. This should return caten/air:graph."))
(defgeneric forward (op &rest tensors)
  (:documentation "Create the type for the Tensor after computation. Be mindful of its lazy evaluation nature; do not perform the actual computation."))
(defgeneric backward (op prev-grad)
  (:documentation "Create the graph for backward of op given prev-grad. Return: `(values input_1.grad input_2.grad ...)`.
save-for-backward is determined automatically, so you do not have to consider about in-place operation."))
(defmethod forward :around ((op Func) &rest tensors)
  (let ((outs (handler-bind
		  ((error
		     #'(lambda (c) (error 'caten-forward-error :op op :inputs tensors :c c))))
		(multiple-value-list (call-next-method)))))
    (setf (func-variables op) tensors)
    (dolist (o outs)
      (assert (tensor-p o) ())
      (setf (tensor-variables o) tensors
	    (tensor-op o) op))
    (apply #'values outs)))
;; ~~ implementations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Allocate (Func) ((buffer :initarg :buffer :type Tensor :accessor alloc-buffer)))
(defmethod forward ((op Allocate) &rest tensors) (declare (ignore tensors)) (alloc-buffer op))
(defmethod backward ((op Allocate) dout)
  (let ((buff (alloc-buffer op)))
    (when (tensor-requires-grad buff)
      ;; op.grad += buff
      (values (!add (tensor-grad buff) dout :reduce t)))))
(defmethod lower ((op Allocate) &rest inputs)
  (declare (ignore inputs))
  (let ((buff (alloc-buffer op))
	(nodes))
    (flet ((->lower (obj) ;; If the shape includes a tensor, it also needs to be lowered
	     (if (or (numberp obj) (symbolp obj)) obj
		 (let ((g (%tensor->aasm obj)))
		   (and (push g nodes) (car (last (graph-nodes g))))))))
      (let ((g
	      (with-context
		(s (map 'list #'->lower (tensor-shape buff)))
		(a (%make-tensor s :dtype (tensor-dtype buff) :order (tensor-order buff) :id (tensor-id buff))))))
	(push g nodes)
	(apply #'make-graph (apply #'append (map 'list #'graph-nodes (reverse nodes))))))))

(defclass Add (Func) ((reduce :initarg :reduce :initform nil :accessor func-reduce)))
(defmethod forward ((op Add) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Add) dout) (values dout dout))
(defmethod lower ((op Add) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%add a b :reduction (func-reduce op))))))

(defclass Mul (Func) ((reduce :initarg :reduce :initform nil :accessor func-reduce)))
(defmethod forward ((op Mul) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Mul) dout)
  (multiple-value-bind (x y) (apply #'values (func-variables op))
    (values (!mul y dout) (!mul x dout))))
(defmethod lower ((op Mul) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%mul a b :reduction (func-reduce op))))))
;; Unary
(defclass Neg (Func) nil)
(defmethod forward ((op Neg) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Neg) dout) (values (!neg dout)))
(defmethod lower ((op Neg) &rest inputs) (with-context (a (%neg (car inputs)))))

(defclass Recip (Func) nil)
(defmethod forward ((op Recip) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Recip) dout)
  (let ((ret (!recip (car (func-variables op)))))
    (values (!mul (!mul (!neg dout) ret) ret)))) ;; -dout / x^2
(defmethod lower ((op Recip) &rest inputs) (with-context (a (%recip (car inputs)))))
;; ~~ wrappers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Functions should start with the prefix !
;; BinaryOps
(declaim (ftype (function (Tensor Tensor &key (:reduce boolean)) (values Tensor &optional)) !add !sub !mul !div))
(defun !add (a b &key (reduce nil)) (forward (make-instance 'Add :reduce reduce) a b))
(defun !mul (a b &key (reduce nil)) (forward (make-instance 'Mul :reduce reduce) a b))
(defun !sub (a b &key (reduce nil)) (!add a (!neg b) :reduce reduce))
(defun !div (a b &key (reduce nil)) (!mul a (!recip b) :reduce reduce))
(macrolet ((def (name b) `(defun ,name (&rest args) (reduce ,b args))))
  (def !+ #'!add)
  (def !- #'!sub)
  (def !* #'!mul)
  (def !/ #'!div))
(macrolet ((def (name cls)
	     `(progn
		(declaim (ftype (function (Tensor) (values Tensor &optional)) ,name))
		(defun ,name (x) (declare (type Tensor x)) (forward (make-instance ',cls) x)))))
  (def !neg Neg)
  (def !recip Recip))

;; UnaryOps

