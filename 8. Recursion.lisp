;;; 8.1
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))

;;; 8.2
(defun anyoddp (x)
  (if x
      (if (oddp (first x))
          t
          (anyoddp (rest x)))))

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))

;;; 8.4
(defun laugh (n)
  (cond ((zerop n) nil)
        (t (cons 'ha (laugh (- n 1))))))

;;; 8.5
(defun add-up (x)
  (cond ((null x) 0)
        (t (+ (first x) (add-up (rest x))))))

;;; 8.6
(defun alloddp (x)
  (cond ((null x) t)
        ((oddp (first x)) (alloddp (rest x)))))

;;; 8.7
(defun rec-member (x lst)
  (cond ((null lst) nil)
        ((equal x (first lst)) t)
        (t (rec-member x (rest lst)))))

;;; 8.8
(defun rec-assoc (x table)
  (cond ((null table) nil)
        ((equal x (caar table)) (first table))
        (t (rec-assoc x (rest table)))))

;;; 8.9
(defun rec-nth (n lst)
  (cond ((null lst) nil)
        ((zerop n) (first lst))
        (t (rec-nth (1- n) (rest lst)))))

;;; 8.10
(defun add1 (x) (+ x 1))
(defun sub1 (x) (- x 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
        (t (rec-plus (add1 x) (sub1 y)))))

;;; 8.11
(defun fib (n)
  (cond ((zerop n) 1)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

;;; 8.17
(defun find-first-odd (x)
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (find-first-odd (rest x)))))

;;; 8.18
(defun last-element (x)
  (cond ((atom (rest x)) (first x))
        (t (last-element (rest x)))))

;;; 8.21
(defun add-nums (n)
  (cond ((zerop n) 0)
        (t (+ n (add-nums (1- n))))))

;;; 8.22
(defun all-equal (x)
  (cond ((or (null x) (< (length x) 2)) t)
        ((equal (first x) (second x))
         (all-equal (rest x)))))

;;; 8.24
(defun count-down (n)
  (cond ((zerop n) nil)
        (t (cons n (count-down (1- n))))))

;;; 8.25
(defun applic-fact (n)
  (reduce #'* (count-down n)))

;;; 8.26
(defun count-down (n)
  (cond ((zerop n) (list 0))
        (t (cons n (count-down (1- n))))))

(defun count-down (n)
  (cond ((equal -1 n) nil)
        (t (cons n (count-down (1- n))))))

;;; 8.27
(defun squre-list (x)
  (cond ((null x) nil)
        (t (cons (* (first x) (first x)) (squre-list (rest x))))))

;;; 8.28
(defun my-nth (n x)
  (cond ((null x) nil)
        ((zerop n) (first x))
        (t (my-nth (1- n) (rest x)))))

;;; 8.29
(defun my-member (x lst)
  (cond ((null lst) nil)
        ((equal x (first lst)) lst)
        (t (my-member x (rest lst)))))

;;; 8.30
(defun my-assoc (key table)
  (cond ((null table) nil)
        ((equal key (caar table)) (first table))
        (t (my-assoc key (rest table)))))

;;; 8.31
(defun compare-lengths (x y)
  (cond ((and (null x) (null y)) 'same-length)
        ((null x) 'second-is-longer)
        ((null y) 'first-is-longer)
        (t (compare-lengths (rest x)
                            (rest y)))))

;;; 8.32
(defun sum-numeric-elements (lst)
  (cond ((null lst) 0)
        ((numberp (first lst))
         (+ (first lst)
            (sum-numeric-elements (rest lst))))
        (t (sum-numeric-elements (rest lst)))))

;;; 8.33
(defun my-remove (x lst)
  (cond ((null lst) nil)
        ((equal x (first lst))
         (my-remove x (rest lst)))
        (t (cons (first lst) (my-remove x (rest lst))))))

;;; 8.34
(defun my-intersection (x y)
  (cond ((null x) nil)
        ((member (first x) y)
         (cons (first x) (my-intersection (rest x) y)))
        (t (my-intersection (rest x) y))))

;;; 8.35
(defun my-set-difference (x y)
  (cond ((null x) nil)
        ((member (first x) y)
         (my-set-difference (rest x) y))
        (t (cons (first x) (my-set-difference (rest x) y)))))

;;; 8.36
(defun count-odd (lst)
  (cond ((null lst) 0)
        ((oddp (first lst)) (1+ (count-odd (rest lst))))
        (t (count-odd (rest lst)))))

(defun count-odd (lst)
  (cond ((null lst) 0)
        (t (+ (if (oddp (first lst))
                1
                0)
              (count-odd (rest lst))))))

;;; 8.37
(defun combine (x y)
  (+ x y))

(defun fib (n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (t (combine (fib (- n 1)) (fib (- n 2))))))

(defun atomic-to-q (x)
  (cond ((null x) nil)
        ((atom x) 'q)
        (t (cons (atomic-to-q (first x))
                 (atomic-to-q (rest x))))))

;;; 8.39
(defun count-atoms (x)
  (cond ((null x) 1)
        ((atom x) 1)
        (t (+ (count-atoms (first x))
              (count-atoms (rest x))))))

;;; 8.40
(defun count-cons (x)
  (cond ((atom x) 0)
        (t (+ 1
              (count-cons (first x))
              (count-cons (rest x))))))

;;; 8.41
(defun sum-tree (x)
  (cond ((null x) 0)
        ((atom x)
         (if (numberp x)
             1
             0))
        (t (+ (sum-tree (first x))
              (sum-tree (rest x))))))

;;; 8.42
(defun my-subst (new old tree)
  (cond ((null tree) nil)
        ((atom tree) (if (equal old tree)
                         new
                         tree))
        (t (cons (my-subst new old (first tree))
                 (my-subst new old (rest tree))))))

;;; 8.43
(defun flatten (tree)
  (cond ((null tree) nil)
        ((atom tree) (list tree))
        (t (append (flatten (first tree))
                   (flatten (rest tree))))))

;;; 8.44
(defun tree-depth (tree)
  (cond ((atom tree) 0)
        (t (+ 1 (max (tree-depth (first tree))
                 (tree-depth (rest tree)))))))

(defun count-up (n)
  (count-up-recursively 1 n))

(defun count-up-recursively (cnt n)
  (cond ((> cnt n) nil)
        (t (cons cnt
                 (count-up-recursively (1+ cnt) n)))))

;;; 8.46
(defun count-up (n)
  (cond ((= 0 n) nil)
        (t (append (count-up (1- n)) (list n)))))

;;; 8.47
(defun make-loaf (n)
  (if (zerop n)
      nil
      (cons 'x (make-loaf (1- n)))))

;;; 8.48
(defun bury (x n)
  (cond ((zerop n) x)
        ( t (list (bury x (1- n))))))

;;; 8.49
(defun pairings (x y)
  (cond ((null x) nil)
        (t (cons
            (list (first x) (first y))
            (pairings (rest x) (rest y))))))

;;; 8.50
(defun sublists (lst)
  (cond ((null lst) nil)
        (t (cons lst
                 (sublists (rest lst))))))

;;; 8.51
(defun my-reverse (lst)
  (reverse-recursively lst nil))

(defun reverse-recursively (x y)
  (cond ((null x) y)
        (t (reverse-recursively
            (rest x)
            (cons (first x) y)))))

;;; 8.52
(defun my-union (x y)
  (append x (union-recursively x y)))

(defun union-recursively (x y)
  (cond ((null y) nil)
        ((member (first y) x)
         (union-recursively x (rest y)))
        (t (cons (first y) (union-recursively x (rest y))))))

;;; 8.53
(defun largest-even (lst)
  (cond ((null lst) 0)
        ((evenp (first lst))
         (max (first lst) (largest-even (rest lst))))
        (t (largest-even (rest lst)))))

;;; 8.54
(defun huge (n)
  (huge-recursive n n))

(defun huge-recursive (x n)
  (cond ((equal 0 n) 1)
        (t (* x (huge-recursive x (1- n))))))

;;; 8.56
(defun every-other (lst)
  (every-other-helper 1 lst))

(defun every-other-helper (n lst)
  (cond ((null lst) nil)
        ((oddp n) (cons (first lst)
                        (every-other-helper (1+ n) (rest lst))))
        (t (every-other-helper (1+ n) (rest lst)))))

;;; 8.57
(defun left-half (lst)
  (left-half-helper lst lst))

(defun left-half-helper (x y)
  (cond ((null x) nil)
        (t (cons (first y)
                 (left-half-helper (cddr x) (rest y))))))

;;; 8.58
(defun merge-lists (x y)
  (cond ((null x) y)
        ((null y) x)
        ((< (first x) (first y))
         (cons (first x)
               (merge-lists (rest x) y)))
        (t (cons (first y) (merge-lists x (rest y))))))

(defun tr-count-slices (loaf)
  (labels ((tr-cs1 (loaf n)
             (cond ((null loaf) n)
                   (t (tr-cs1 (rest loaf) (1+ n))))))
    (tr-cs1 loaf 0)))

(defun tr-reverse (x)
  (labels ((tr-rev1 (x result)
             (cond ((null x) result)
                   (t (tr-rev1
                       (rest x)
                       (cons (first x) result))))))
    (tr-rev1 x nil)))

;;; 8.61
(defun tr-count-up (n)
  (tr-count-np1 n nil))

(defun tr-count-np1 (n result)
  (cond ((zerop n) result)
        (t (tr-count-np1
            (1- n)
            (cons n result)))))

(defun tr-count-up (n)
  (labels ((rec (cnt)
             (cond ((> cnt n) nil)
                   (t
                    (cons cnt
                          (rec (1+ cnt)))))))
    (rec 1)))

;;; 8.61
(defun tr-count-up (n)
  (count-recursive n nil))

(defun count-recursive (n result)
  (cond ((zerop n) result)
        (t (count-recursive (1- n) (cons n result)))))

;;; 8.62
(defun tr-fact (n)
  (fact-recursive n 1))

(defun fact-recursive (n result)
  (cond ((zerop n) result)
        (t (fact-recursive (1- n) (* n result)))))

;;; 8.63
(defun tr-union (x y)
  (cond ((null x) y)
        ((member (first x) y) (tr-union (rest x) y))
        (t (tr-union (rest x) (cons (first x) y)))))

(defun tr-intersection (x y)
  (intersecion-helper x y nil))

(defun intersecion-helper (x y result)
  (cond ((null x) result)
        ((member (first x) y)
         (intersecion-helper (rest x) y (cons (first x) result)))
        (t (intersecion-helper (rest x) y result))))

(defun tr-set-difference (x y)
  (set-difference-helper x y nil))

(defun set-difference-helper (x y result)
  (cond ((null x) result)
        ((member (first x) y) (set-difference-helper (rest x) y result))
        (t (set-difference-helper (rest x) y (cons (first x) result)))))

;;; writing new applicative operators
(defun my-mapcar (fn x)
  (cond ((null x) nil)
        (t (cons (funcall fn (first x))
                 (my-mapcar fn (rest x))))))

;;; 8.64
(defun tree-find-if (fn tree)
  (cond ((null tree) nil)
        ((atom tree) (if (funcall fn tree) tree))
        (t (or (tree-find-if fn (first tree))
               (tree-find-if fn (rest tree))))))
;;; 8.65
(defun tr-count-slices (x)
  (labels ((rec (x n)
             (if x
                 (rec (rest x) (1+ n))
                 n)))
    (rec x 0)))

(defun tr-reverse (x)
  (labels ((rec (x result)
             (if x
                 (rec (rest x) (cons (first x) result))
                 result)))
    (rec x nil)))

;;; recursive data structures
;;; 8.66
(defun arith-eval (expr)
  (cond ((numberp expr) expr)
        (t (funcall (second expr)
                    (arith-eval (first expr))
                    (arith-eval (third expr))))))

;;; 8.70

;;; 8.71
