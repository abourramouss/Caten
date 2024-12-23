(in-package :caten/nn)

(defmodel (Sigmoid () :where "A[~] -> A[~]") ((ret)))
(defmethod call ((op Sigmoid) &rest inputs)
  (let ((x (car inputs)))
    (setf (slot-value op 'ret) (!recip (!add (fconst 1 :dtype (dtype-of x)) (!exp2 (!mul x (fconst (/ -1 (log 2)) :dtype (dtype-of x)))))))
    (slot-value op 'ret)))
(defmethod backward ((op Sigmoid) &optional prev-dout)
  (let ((ret (slot-value op 'ret)))
    (!mul (!mul ret (!add (fconst 1 :dtype (dtype-of ret)) (!neg ret))) prev-dout)))
(defun !sigmoid (x)
  "
```
(!sigmoid x)
```
Applies the Sigmoid function element-wise to the input tensor.
"
  (forward (Sigmoid) x))
;; Implements: https://github.com/onnx/onnx/blob/main/docs/Changelog.md#hardsigmoid-22
(defmodel (HardSigmoid (&key (alpha 0.2) (beta 0.5)) :where "A[~] -> A[~]") ((alpha alpha) (beta beta)))
(defmethod call ((op HardSigmoid) &rest inputs)
  (let ((x (car inputs)))
    (with-slots ((alpha alpha) (beta beta)) op
      (!maximum (!const x 0) (!minimum (!const x 1) (!add (!mul (!const x alpha) x) (!const x beta)))))))
(defun !hard-sigmoid (x &key (alpha 0.2) (beta 0.5))
  "
```
(!hard-sigmoid x &key (alpha 0.2) (beta 0.5))
```
Applies the HardSigmoid function element-wise to the input tensor.
"
  (forward (HardSigmoid :alpha alpha :beta beta) x))

(defmodel (ReLU () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU) &rest inputs)
  (!maximum (car inputs) (!const (car inputs) 0)))
(defun !relu (x)
  "
```
(!relu x)
```
Applies the ReLU function element-wise to the input tensor.
"
  (forward (ReLU) x))

(defmodel (LeakyReLU (&key (neg-slope 1e-3)) :where "A[~] -> A[~]") ((neg-slope neg-slope)))
(defmethod call ((op LeakyReLU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((neg-slope neg-slope)) op
      (!sub (!relu x) (!relu (!mul x (!neg (fconst neg-slope :dtype (dtype-of x)))))))))
(defun !leaky-relu (x &key (neg-slope 1e-3))
  "
```
(!leaky-relu x &key (neg-slope 1e-3))
```
Applies the LeakyReLU function element-wise to the input tensor."
  (forward (LeakyReLU :neg-slope neg-slope) x))

(defun _softmax (x &key (axis -1))
  (let* ((m (!sub x (!max x :axis axis)))
	 (e (!exp m)))
    (values m e (!sum e :axis axis))))

(defmodel (LogSoftmax (&key (axis -1)) :where "A[~] -> A[~]") ((axis axis)))
(defmethod call ((op LogSoftmax) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((axis axis)) op
      (multiple-value-bind (m e ss) (_softmax x :axis axis)
	(declare (ignore e))
	(!sub m (!log ss))))))
(defun !log-softmax (x &key (axis -1))
  "
```
(!log-softmax x &key (axis -1))
```
Applies the LogSoftmax function element-wise to the input tensor.
"
  (forward (LogSoftmax :axis axis) x))

(defmodel (ELU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op ELU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((alpha alpha)) op
      (!sub (!relu x) (!relu (!mul (!const x alpha) (!sub (!const x 1) (!exp x))))))))
(defun !elu (x &key (alpha 1.0))
  "
```
(!elu x &key (alpha 1.0))
```
Applies the ELU function element-wise to the input tensor.
"
  (forward (ELU :alpha alpha) x))

(defmodel (ReLU6 () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU6) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (!sub (!relu x) (!relu (!sub x (!const x 6))))))
(defun !relu6 (x)
  "
```
(!relu6 x)
```
Applies the ReLU6 function element-wise to the input tensor.
"
  (forward (ReLU6) x))

(defmodel (Softmax (&key (axis -1)) :where "A[~] -> A[~]") ((axis axis :accessor softmax-axis)))
(defmethod call ((op Softmax) &rest inputs)
  (multiple-value-bind (m e ss) (_softmax (car inputs) :axis (softmax-axis op))
    (declare (ignore m))
    (!div e ss)))
(defun !softmax (x &key (axis -1))
  "
```
(!softmax x &key (axis -1))
```
Applies the Softmax function to the input tensor.
"
  (forward (Softmax :axis axis) x))

(defmodel (Softplus (&key (beta 1.0)) :where "A[~] -> A[~]") ((beta beta)))
(defmethod call ((op Softplus) &rest inputs)
  (with-slots ((beta beta)) op
    (!mul (!const (car inputs) (/ 1 beta)) (!log (!add (!const (car inputs) 1) (!exp (!mul (car inputs) (!const (car inputs) beta))))))))
(defun !softplus (x &key (beta 1.0))
  "
```
(!softplus x &key (beta 1.0))
```
Applies the Softplus function element-wise to the input tensor.
"
  (forward (SoftPlus :beta beta) x))

(defmodel (Softsign () :where "A[~] -> A[~]") ())
(defmethod call ((op Softsign) &rest inputs)
  (let ((x (car inputs)))
    (!div x (!+ (!const x 1) (!abs x)))))
(defun !softsign (x)
  "
```
(!softsign x)
```
Applies the Softsign function element-wise to the input tensor.
"
  (forward (Softsign) x))

(defmodel (SoftShrink (&key (lmd 0.5)) :where "A[~] -> A[~]")
  ((lmd lmd)))
(defmethod call ((op SoftShrink) &rest inputs)
  (let* ((x (car inputs))
         (lmd-tensor (!const x (slot-value op 'lmd))))
    (declare (type tensor x))
    (!where (!> (!abs x) lmd-tensor)
            (!sub x (!mul (!signum x) lmd-tensor))
            (!const x 0))))
(defun !softshrink (x &key (lmd 0.5))
  "
```
(!softshrink x &key (lmd 0.5))
```
Applies the SoftShrink function element-wise to the input tensor.
"
  (forward (SoftShrink :lmd lmd) x))

(defmodel (CeLU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op CeLU) &rest inputs)
  (let ((x (car inputs))
	(alpha (slot-value op 'alpha)))
    (declare (type tensor x))
    (!add (!maximum x (!const x 0)) (!minimum (!const x 0) (!mul (!const x alpha) (!sub (!exp (!/ x (!const x alpha))) (!const x 1)))))))
(defun !celu (x &key (alpha 1.0))
  "
```
(!celu x &key (alpha 1.0))
```
Applies the CeLU function element-wise to the input tensor.
"
  (forward (CeLU :alpha alpha) x))

(defmodel (SiLU () :where "A[~] -> A[~]") ())
(defmethod call ((op SiLU) &rest inputs &aux (x (car inputs))) (!mul x (!sigmoid x)))
(defun !silu (x)
  "
```
(!silu x)
```
Applies the SiLU function element-wise to the input tensor.
"
  (forward (SiLU) x))

(defmodel (LogSigmoid () :where "A[~] -> A[~]") ())
(defmethod call ((op LogSigmoid) &rest inputs &aux (x (car inputs))) (!neg (!softplus (!neg x ))))
(defun !logsigmoid (x)
  "
```
(!logsigmoid x)
```
Applies the LogSigmoid function element-wise to the input tensor.
"
  (forward (LogSigmoid) x))

(defmodel (GeLU (&key (approx :tanh)) :where "A[~] -> A[~]") ((approx approx)))
(defmethod gelu/call ((op GeLU) (approx (eql :tanh)) x)
  (!* (!const x 0.5) x (!+ (!const x 1) (!tanh (!* (!const x (sqrt (/ 2.0 pi))) (!+ x (!* (!const x 0.044715) (!* x x x))))))))
(defmethod gelu/call ((op GeLU) (approx (eql :sigmoid)) x)
  (!mul x (!sigmoid (!* (!const x 1.702) x))))
(defmethod call ((op GeLU) &rest inputs) (gelu/call op (slot-value op 'approx) (car inputs)))
(defun !gelu (x &key (approx :tanh))
  "
```
(!gelu x &key (approx :tanh))
```
Applies the GeLU activation to the input tensor. There are two ways to approximate the GeLU function. `:tanh` to use tanh. `:sigmoid` to use sigmoid.
"
  (forward (GeLU :approx approx) x))

(defmodel (SeLU () :where "A[~] -> A[~]") ())
(defmethod call ((op SeLU) &rest inputs &aux (x (car inputs))) (!mul (!elu x :alpha 1.67326) (!const x 1.0507)))
(defun !selu (x)
  "
```
(!selu x)
```
Applies the SeLU function element-wise to the input tensor.
"
  (forward (SeLU) x))

(defmodel (Mish () :where "A[~] -> A[~]") ())
(defmethod call ((op Mish) &rest inputs &aux (x (car inputs))) (!mul x (!tanh (!softplus x))))
(defun !mish (x)
  "
```
(!mish x)
```
Applies the Mish function element-wise to the input tensor.
"
  (forward (Mish) x))

(defmodel (HardSwish () :where "A[~] -> A[~]") ())
(defmethod call ((op HardSwish) &rest inputs &aux (x (car inputs)))
  (!* x (!relu6 (!add x (!const x 3))) (!const x (/ 1 6))))
(defun !hardswish (x)
  "
```
(!hardswish x)
```
Applies the HardSwish function element-wise to the input tensor.
"
  (forward (HardSwish) x))

(defmodel (HardTanh (&key (min_val -1.0) (max_val 1.0)) :where "A[~] -> A[~]") ((min_val min_val) (max_val max_val)) )
(defmethod call ((op HardTanh) &rest inputs)
  (let ((x (car inputs))
        (min_val (slot-value op 'min_val))
        (max_val (slot-value op 'max_val)))
    (!minimum (!maximum x (!const x min_val)) (!const x max_val))))
(defun !hardtanh (x &key (min_val -1.0) (max_val 1.0))
  "
```
(!hardtanh x &key (min_val -1.0) (max_val 1.0))
```
Applies the HardTanh function element-wise to the input tensor.
"
  (forward (HardTanh :min_val min_val :max_val max_val) x))

(defmodel (Softmin () :where "A[~] -> A[~]") ())
(defmethod call ((op Softmin) &rest inputs &aux (x (car inputs))) (!softmax (!neg x)))
(defun !softmin (x)
  "
```
(!softmin x)
```
Applies the Softmin function element-wise to the input tensor.
"
  (forward (Softmin) x))

(in-package :caten/nn.test)
;; TODO: Implement Assert Close, printing atol/rtol
(defun sigmoid-lisp (x) (/ (+ 1 (expt 2 (* x (/ -1 (log 2)))))))
(define-nn-test Sigmoid
  "Testing w/ Sigmoid([100, 100])"
  :compile (caten (!sigmoid (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'sigmoid-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun hardsigmoid-lisp (x alpha beta) (max 0 (min 1 (+ (* alpha x) beta))))
(define-nn-test HardSigmoid
  "Testing w/ HardSigmoid([100, 100])"
  :compile (caten (!hard-sigmoid (make-tensor `(100 100) :from 'x) :alpha 0.2 :beta 0.5))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun relu-lisp (x) (max x 0.0))
(define-nn-test ReLU
  "Testing w/ ReLU([100, 100])"
  :compile (caten (!relu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 1 -300)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu-lisp x))))
  :assert-close ((x y) (every #'= x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun leaky-relu-lisp (x) (- (relu-lisp x) (relu-lisp (* x (- 1e-3)))))
(define-nn-test Leaky-ReLU
  "Testing w/ Leaky-ReLU([100, 100])"
  :compile (caten (!leaky-relu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'leaky-relu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test Softmax
  "Testing w/ Softmax([512, 256])"
  :compile (caten (!softmax (make-tensor `(512 256) :from 'x)))
  :inputs (ctx:with-contextvar (:jit 0 :avm :lisp)
	    (list (proceed (!rand `(512 256)))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!softmax x)))
  :assert-close ((x y)
		 (let ((sum (proceed (!contiguous (!sum x :axis -1)))))
		   (every #'(lambda (x) (<= (abs (- x 1.0)) 1e-1)) (elements sum)))
		 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (and
		      (= 2 (n-args `(512 256) model))
		      (= 0 (n-args `(512 1) model))))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test LogSoftmax
  "Testing w/ LogSoftmax([512, 256])"
  :compile (caten (!log-softmax (make-tensor `(512 256) :from 'x)))
  :inputs (ctx:with-contextvar (:jit 0 :avm :lisp)
	    (list (proceed (!rand `(512 256)))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!log-softmax x)))
  :assert-close ((x y)
		 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (= 2 (n-args `(512 256) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun elu-lisp (x &aux (alpha 1.0)) (- (relu-lisp x) (relu-lisp (* alpha (- 1 (exp x))))))
(define-nn-test ELU
  "Testing w/ ELU([100, 100])"
  :compile (caten (!elu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'elu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun relu6-lisp (x) (- (relu-lisp x) (relu-lisp (- x 6))))
(define-nn-test ReLU6
  "Testing w/ ReLU6([100, 100])"
  :compile (caten (!relu6 (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu6-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softplus-lisp (x &aux (beta 1.0))
  (* (/ 1 beta) (log (+ 1 (exp (* x beta))))))
(define-nn-test SoftPlus
  "Testing w/ SoftPlus([100, 100])"
  :compile (caten (!softplus (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softplus-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softsign-lisp (x) (/ x (+ 1 (abs x))))
(define-nn-test SoftSign
  "Testing w/ SoftSign([100, 100])"
  :compile (caten (!softsign (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.01 1.0)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softsign-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softshrink-lisp (x &aux (lmd 0.5))(cond ((> x lmd) (- x lmd))((< x (- lmd)) (+ x lmd))(t 0)))
(define-nn-test SoftShrink
  "Testing w/ SoftShrink([100, 100])"
  :compile (caten (!softshrink (make-tensor `(100 100) :from 'x)))
  :inputs (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softshrink-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun celu-lisp (x &aux (alpha 1.0)) (+ (max x 0.0) (min 0 (* alpha (- (exp (/ x alpha)) 1)))))
(define-nn-test CeLU
  "Testing w/ CeLU([100, 100])"
  :compile (caten (!celu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 1)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'celu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun silu-lisp (x) (* x (sigmoid-lisp x)))
(define-nn-test SiLU
  "Testing w/ SiLU([100, 100])"
  :compile (caten (!silu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'silu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))


(defun logsigmoid-lisp (x)(log (/ 1 ( + 1 (exp (- x))))))
(define-nn-test LogSigmoid
  "Testing w/ LogSigmoid([100, 100])"
  :compile (caten (!logsigmoid (make-tensor `(100 100) :from 'x)))
  :inputs (list (proceed (ax+b `(100 100) 0.01 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'logsigmoid-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun gelu-lisp (x) (* 0.5 x (+ 1 (tanh (* (sqrt (/ 2.0 (coerce pi (type-of x)))) (+ x (* 0.044715 (* x x x))))))))
(define-nn-test GeLU
  "Testing w/ GeLU([100, 100])"
  :compile (caten (!gelu (make-tensor `(100 100) :from 'x) :approx :tanh))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'gelu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun selu-lisp (x &aux (lambda 1.0507) (alpha 1.67326))(* lambda (if (>= x 0) x (* alpha (- (exp x) 1)))))
(define-nn-test SeLU
  "Testing w/ SeLU([100, 100])"
  :compile (caten (!selu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'selu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun mish-lisp (x) (* x (tanh (log (+ 1 (exp x))))))
(define-nn-test Mish
  "Testing w/ Mish([100, 100])"
  :compile (caten (!mish (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 7)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'mish-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun hardswish-lisp (x) (* x (relu6-lisp (+ x 3.0)) (/ 1 6)))
(define-nn-test HardSwish
  "Testing w/ HardSwish([100, 100])"
  :compile (caten (!hardswish (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'hardswish-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))


(defun hardtanh-lisp (x &aux (min_val -1.0) (max_val 1.0)) (cond ((> x max_val) max_val) ((< x min_val) min_val) (t x)))
(define-nn-test HardTanh
  "Testing w/ HardTanh([100, 100])"
  :compile (caten (!hardtanh (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'hardtanh-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test Softmin
  "Testing w/ Softmin([512, 256])"
  :compile (caten (!softmin (make-tensor `(512 256) :from 'x)))
  :inputs (ctx:with-contextvar (:jit 0 :avm :lisp)
            (list (proceed (!rand `(512 256)))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!softmin x)))
  :assert-close ((x y)
                 (let ((sum (proceed (!contiguous (!sum x :axis -1)))))
                   (every #'(lambda (x) (<= (abs (- x 1.0)) 1e-1)) (elements sum)))
                 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (and
                      (= 2 (n-args `(512 256) model))
                      (= 0 (n-args `(512 1) model))))
  :kernel   ((model) (= 1 (n-kernels model))))

