(in-package :caten/avm)
(defgeneric %impl (device op graph node args) (:documentation "Peforms the corresponding nodes"))

(defun make-hash-table-from-params (params)
  (declare (type list params))
  (let ((out (make-hash-table :test #'eql)))
    (loop for (k . v) in params
	  do (setf (gethash k out) v))
    out))
(defstruct (AVM
	    (:constructor make-avm (graph &optional params)))
  "Tape based iseq executor"
  (graph graph :type graph)
  (tape-length (length (graph-nodes graph)) :type fixnum)
  (pc 0 :type fixnum)
  (variables (make-hash-table-from-params params) :type hash-table))

(defun vm/readvar (avm id)
  (declare (type avm avm)
	   (type symbol id))
  (or (gethash id (avm-variables avm))
      (error "AVM Runtime Error: ~a is not defined in ~a" id avm)))
(defun vm/setvar (avm id value)
  (declare (type avm avm)
	   (type symbol id)
	   (type Buffer value))
  (setf (gethash id (avm-variables avm)) value))
(defun vm/step (avm)
  (declare (type avm avm))
  (let ((node (nth (avm-pc avm) (graph-nodes (avm-graph avm)))))
    (declare (type node node))
    (flet ((->real (x)
	     (if (symbolp x)
		 (vm/readvar avm x)
		 x)))
      (let ((type   (node-type node))
	    (writes (node-writes node))
	    (reads  (node-reads node)))
        (let ((out (multiple-value-list
		    (handler-bind ((error #'(lambda (cond) (error 'avm-runtime-error :avm avm :cond cond))))
		      (%impl *device* type (avm-graph avm) node (map 'list #'->real reads))))))
	  (assert (= (length out) (length writes)))
	  (loop for real in out
		for place in writes
		do (vm/setvar avm place real))
	  ;; Move to the next tape
	  (incf (avm-pc avm))
	  (map 'list #'->real writes))))))
(defun vm/run (avm)
  (declare (type avm avm))
  (let ((final-result))
    (loop while (< (avm-pc avm) (avm-tape-length avm)) do (setf final-result (vm/step avm)))
    (apply #'values final-result)))
(defun %realize (graph &key (params))
  "params: ((key . value) ...) "
  (declare (type graph graph))
  (vm/run (make-avm graph params)))