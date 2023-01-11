(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))


;;; let command includes an implicit progn command, and
;;; it can have multiple Lisp instructions inside.
(let1 foo (+ 2 3)
           (* foo foo))

(let1 foo (+ 2 3)
  (princ "lisp is awesome!")
  (* foo foo))

(macroexpand '(let1 foo (+ 2 3)
               (* foo foo)))

(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

(split '(2 3)
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))

(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

(defmacro split (val yes no)
  `(let1 x, val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

(split (progn (princ "Lisp rocks!")
              '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be splict."))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(macroexpand '(split '(2 3)
               (+ x head)
               nil))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))


(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d~%" p))


(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
      ,@body)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
                  ((> ,var ,end))
                ,@body)))

(do-primes (p 0 10)
  (print p))
