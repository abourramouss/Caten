(in-package :caten/air)

(defstruct (Graph
	    (:constructor make-graph (&rest nodes)))
  (nodes nodes :type list))
(defun graph-collect-if (graph f)
  (declare (type graph graph)
	   (type function f))
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (funcall f node) collect node)))
(defun id->users (graph id) (graph-collect-if graph #'(lambda (node) (find id (node-reads node) :test #'eql))))
(defun id->value (graph id)
  (let ((result (graph-collect-if graph #'(lambda (node) (find id (node-writes node) :test #'eql)))))
    (assert (<= (length result) 1) () "write rules must be immutable. ~a" graph)
    (car result)))
(defun remnode (graph id)
  (declare (type graph graph)
	   (type symbol id))
  (setf (graph-nodes graph)
	(loop for node in (graph-nodes graph)
	      unless (eql id (node-id node)) collect node)))
(defun verify-graph (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph)
	do (id->value graph (node-id node)))
  t)