;; A lazy lisp

(defmacro thunk (&body body)
  `(lambda () ,@body))

(defmacro lambdas (vars &body body)
  (if (null (cdr vars))
      `(lambda (,(car vars)) ,@body)
      `(lambdas (,(car vars))
         (lambdas ,(cdr vars) ,@body))))

(defun force (f)
  (funcall f))

(defun rl-eval (exp env)
  (thunk
    (cond ((symbolp exp) (lookup exp env))
          ((or (numberp exp) (stringp exp)) exp)
          ((consp exp)
           (case (car exp)
             ((quote) (cadr exp))
             (t (apply #'rl-apply
                       (rl-eval (car exp) env)
                       (mapcar (lambda (ex1) (rl-eval ex1 env)) (cdr exp)))))))))

(defun lookup (var env)
  (if (null env)
      (error "No such variable")
      (let ((res (lookup1 var (car env))))
        (if (consp res)
            (cdr res)
            (lookup var (cdr env))))))

(defun lookup1 (var env)
  (cond ((null env) nil)
        ((eq var (caar env)) (car env))
        (t (lookup1 var (cdr env)))))

(defun rl-apply (f &rest xs)
  (if (null xs)
      (force f)
      (apply #'rl-apply
             (thunk (funcall (force f) (car xs)))
             (cdr xs))))

;; (defun rl-apply (f x)
;;   (funcall (force f) x))

(defparameter *default-env*
  `(((t . t)
     (nil . nil)
     (+ . ,(lambda (x) (lambda (y) (+ (force x) (force y)))))
     (if . ,(lambdas (test tr fl)
                     (if (force test)
                         (force tr)
                         (force fl))))
     (error . ,(lambda (x) (error (force x)))))))

