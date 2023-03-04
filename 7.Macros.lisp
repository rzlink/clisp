(defmacro nil! (var)
  (list 'setq var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(mapcar #'(lambda (x)
            (nif x 'p 'z 'n ))
        '(0 2.5 -8))

(defmacro our-when (test &body body)
  `(if ,test
       (progn
         ,@body)))

(defun greet (name)
  `(hello ,name))

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))

(intern "f1oo")


(symbol-value (intern "foo"))

(defmacro make-is-integral-multiple-of (n)
  (let ((function-name (intern (concatenate
                                'string
                                (symbol-name :is-integral-multiple-of- )
                                (write-to-string n)))))
    `(defun ,function-name (x)
       (equal 0 (mod x, n)))))


(let ((sym (make-symbol "temp")))
  (setf (symbol-value sym) 42)
  (print sym))

(let ((sym (make-symbol "temp")))
  (setf (symbol-value sym) 42)
  (print (symbol-value sym)))

(setq foo 14)

(symbol-value 'foo)

(defvar foo 13)

(defvar bar 42)

(print (symbol-name 'bar))
(print (symbol-value 'bar))
(print bar)

(symbol-function 'make-is-integral-multiple-of)

(function-lambda-expression 'greet)

(let ((sym 'foo))
  (print (sxhash sym)))

(print (sxhash foo))

(defvar a 1)
(defvar b 1)
(eq a b)
(print (sxhash a))
(print (sxhash b))
(eq a b)

(let ((a (list 1 2 3))
      (b (list 1 2 3)))
  (list (eql a b) (eql a a)))


(defun how-alike (a b)
  (cond ((equal a b) 'the-same)
        ((and (oddp a) (oddp b)) 'both-odd)
        ((and (not (oddp a)) (not (oddp b))) 'both-even)
        ((and (< a 0) (< b 0)) 'both-negative)
        (t 'not-alike)))

(defun same-sign (x y)
  (or (and (zerop x) (zerop y))
      (and (< x 0) (< y 0))
      (and (> x 0) (> y 0))))


(defun count-zeros (x)
  (length (remove-if-not #'zerop x)))


(defun count-odd (lst)
