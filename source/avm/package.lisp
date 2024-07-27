(cl:in-package :cl-user)
(defpackage :caten/avm
  (:use :cl :caten/aasm :caten/air :alexandria)
  ;; from buffer.lisp
  (:export
   #:*device*
   #:*max-display-len*
   #:with-device
   #:Buffer #:copy-buffer #:buffer-p #:buffer-nrank #:buffer-value #:buffer-dtype #:buffer-shape #:buffer-stride #:buffer-views
   #:%vm/allocate-buffer
   #:%vm/read-index
   #:realize-buffer
   #:pprint-buffer)
  ;; from helpers.lisp
  (:export
   #:parse-allocate-node
   #:parse-view-node)
  ;; from opset.lisp
  (:export
   #:%impl
   )
  ;; from runtime.lisp
  (:export
   #:AVM
   #:make-avm #:avm-graph #:avm-name #:avm-fw-outputs #:avm-bw-outputs
   #:avm-id2tensor #:avm-tape-length #:avm-pc #:avm-variables
   #:*vm*
   #:%realize
   #:vm/readvar
   #:vm/setvar
   #:vm/step
   #:vm/forward
   #:vm/backward
   #:vm/set-params)
  )
