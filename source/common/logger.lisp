(cl:in-package :cl-user)
(defpackage :caten/common.logger
  (:documentation "")
  (:nicknames #:log)
  (:use :cl :cl-ansi-text)
  (:export
   #:*default-stream*
   #:make-ordinal-suffix
   #:maybe-ansi
   #:print-error
   #:print-info
   #:print-progress
   #:white-bright
   #:with-progress))

(in-package :caten/common.logger)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *default-stream* t "A default stream to put out logs")
  (macrolet ((defcolor (name color &key (effect :unset) (style :foreground))
	       `(defun ,name (txt)
		  (with-output-to-string (out)
		    (with-color (,color :stream out :effect ,effect :style ,style)
		      (write-string txt out))))))
    (defcolor gray 90)
    (defcolor white-bright :white :effect :bright))

  (defun getenv (name)
    (uiop:symbol-call :caten/common.contextvar :getenv name))
  
  (defmacro maybe-ansi (op &rest args)
    `(if (= 1 (getenv :COLOR))
	 (,op ,@args)
	 ,@args))

  (defun timestamp ()
    (multiple-value-bind
          (second minute hour day month year day-of-week dst-p tz)
	(get-decoded-time)
      (declare (ignore day-of-week dst-p))
      (maybe-ansi
       blue
       (format nil "[~2,'0d:~2,'0d:~2,'0d, ~d/~2,'0d/~d (GMT~@d)]"
	       hour
	       minute
	       second
	       month
	       day
	       year
	       (- tz)))))

  (defun print-info (content &rest args)
    (when (>= (getenv :DEBUG) 0)
      (format *default-stream* "~a : ~a~%" (timestamp) (apply #'format nil content args))))
  
  (defun print-error (content &rest args)
    (format *default-stream* "~a ~a~%" (maybe-ansi red "[ERROR]") (apply #'format nil content args)))
  
  ;; (1/n) ... (xxx ms)
  ;; (2/n) ... (yyy ms)
  (defparameter *progress* nil)
  (defstruct (Progress
	      (:constructor make-progress (total-count &key (debug 0) (timeit t))))
    (debug debug) (n 0) (timeit timeit)
    (total total-count)
    (last-time (get-internal-run-time)))

  (defmacro with-progress ((total &key (title nil) (debug 0) (timeit t)) &body body)
    `(let ((*progress* (make-progress ,total :debug ,debug :timeit ,timeit)))
       (when ,title
	 (format *default-stream* (maybe-ansi white-bright (format nil "~a (n/~a)~%" ,title ,total))))
       ,@body))

  (defun print-progress (content &rest args)
    (when (>= (getenv :DEBUG) 0)
      (assert *progress* () "print-progress: *progress* was not initialized.")
      (let* ((last-time (progress-last-time *progress*))
	     (now (get-internal-run-time))
	     (ms (- now last-time)))
	(setf (progress-last-time *progress*) now)
	(incf (progress-n *progress*))
	(flet ((render-time ()
                 (if (progress-timeit *progress*)
		     (if (or (= 0 ms) (>= ms 1000))
		         (maybe-ansi gray (format nil "(~a sec)" (float (/ ms (* 1000 1000)))))
		         (maybe-ansi gray (format nil "(~a ms)" ms)))
                     "")))
	  (format *default-stream* "~a~%* ~a ~a~a~%"
		  (if (= 1 (progress-n *progress*))
		      ""
		      (render-time))
		  (maybe-ansi blue (format nil "(~a/~a)" (progress-n *progress*) (progress-total *progress*)))
		  (maybe-ansi white-bright (apply #'format nil content args))
		  (if (= (progress-n *progress*) (progress-total *progress*))
		      (format nil " ~a~%" (render-time))
		      "")))))))

(defun make-ordinal-suffix (n)
  (let ((ordinal-suffix (cond ((or (= (mod n 100) 11)
                           (= (mod n 100) 12)
                           (= (mod n 100) 13))
		       "th")
                      ((= (mod n 10) 1) "st")
                      ((= (mod n 10) 2) "nd")
                      ((= (mod n 10) 3) "rd")
                      (t "th"))))
    (format nil "~A~A" n ordinal-suffix)))
