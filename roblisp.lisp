; A lazy lisp

(declaim (optimize (speed 0) (debug 3)))

;; util

(defmacro thunk (&body body)
  `(lambda () ,@body))

(defmacro lambdas (vars &body body)
  (if (null (cdr vars))
      `(lambda (,(car vars)) ,@body)
      `(lambdas (,(car vars))
         (lambdas ,(cdr vars) ,@body))))

(defun force (f)
  (funcall f))

(defun take (n lst)
  (if (= n 0)
      nil
      (cons (car lst) (take (1- n) (cdr lst)))))

;; impl

(defun rl-eval (exp env)
  (thunk
    (cond ((symbolp exp) (force (lookup exp env)))
          ((or (numberp exp) (stringp exp)) exp)
          ((consp exp)
           (case (car exp)
             ((quote) (cadr exp))
             ((lambda) `(&proc ,env ,(cadr exp) ,(caddr exp)))
             (t (let ((exp1 (normalize-call exp)))
                  (rl-apply (rl-eval (car exp1) env)
                            (rl-eval (cadr exp1) env)))))))))

(defun lookup (var env)
  (cond ((null env)
         (error "No such variable"))
        ((eq (caar env) var)
         (cdar env))
        (t (lookup var (cdr env)))))

(defun bind (var exp env)
    (cons (cons var exp) env))

(defun rl-apply (f x)
  (let ((f (force f)))
    (if (functionp f)
        (funcall f x)
        (force
         (rl-eval (cadddr f)
                  (bind (caddr f) x (cadr f)))))))

(defun normalize-call (fc)
  (reduce (lambda (x acc) (list x acc)) (cddr fc) :initial-value (take 2 fc)))

(defparameter *default-env*
  (mapcar (lambda (k) (cons (car k) (thunk (cdr k))))
          `((t . t)
            (nil . nil)
            (+ . ,(lambda (x) (lambda (y) (+ (force x) (force y)))))
            (if . ,(lambdas (test tr fl)
                            (if (force test)
                                (force tr)
                                (force fl))))
            (error . ,(lambda (x) (error (force x)))))))

