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

(defun drop (n lst)
  (if (= n 0)
      lst
      (drop (1- n) (cdr lst))))

(defun droplast (n lst)
  (take (- (length lst) n) lst))

(defun index-list (n f)
  (if (= n 0)
      nil
      (cons (funcall f n) (index-list (1- n) f))))

(defun group (n lst)
  (cond ((null lst) nil)
        ((null (nth n lst))
         (list lst))
        (t (cons (take n lst)
                 (group n (drop n lst))))))

(defun break-at (v)
  (break)
  v)

;; impl
;;; (lambda arg body)
;;; (let var1 val1 var2 val2 body)
(defun rl-eval (exp env)
  (thunk
    (cond ((symbolp exp) (force (lookup exp env)))
          ((or (numberp exp) (stringp exp)) exp)
          ((consp exp)
           (case (car exp)
             ((quote) (cadr exp))
             ((lambda) `(&proc ,env ,(cadr exp) ,(caddr exp)))
             ((let) (force
                     (rl-eval (car (last exp))
                              (let-bind (droplast 1 (cdr exp)) env))))
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

(defun let-bind (varlist env)
  (let ((binds (reduce (lambda (env1 var) (bind var nil env1))
                       (mapcar #'car (group 2 varlist))
                       :initial-value env)))
    (do ((vars (mapcar #'cadr (group 2 varlist)) (cdr vars))
         (env1 binds (cdr env1)))
        ((null vars) binds)
      (setf (cdar env1) (rl-eval (car vars) binds)))))

(defun rl-apply (f x)
  (let ((f (force f)))
    (if (functionp f)
        (funcall f x)
        (force
         (rl-eval (fourth f)
                  (bind (third f) x (second f)))))))

(defun normalize-call (fc)
  (reduce (lambda (x acc) (list x acc))
          (cddr fc)
          :initial-value (take 2 fc)))

(defparameter *default-env*
  (mapcar (lambda (k) `(,(car k) . ,(thunk (cdr k))))
          `((t . t)
            (nil . nil)
            (cons . ,(lambdas (x y) (cons x y)))
            (car . ,(lambda (c) (car (force c))))
            (cdr . ,(lambda (c) (cdr (force c))))
            (+ . ,(lambda (x) (lambda (y) (+ (force x) (force y)))))
            (if . ,(lambdas (test tr fl)
                            (if (force test)
                                (force tr)
                                (force fl))))
            (error . ,(lambda (x) (error (force x)))))))

(defun y (f)
  ((lambda (x) (funcall f (funcall x x)))
   (lambda (y)
     (funcall f (lambda (&rest args)
                  (apply (funcall y y) args))))))

(defun y (f)
  (funcall f (y f)))

(defun fac (f)
  (lambda (n)
    (if (zerop n)
	1
	(* n (funcall f (1- n))))))
