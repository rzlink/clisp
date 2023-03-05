(dotimes (i 4)
  (format t "~&I is ~S." i))

(dolist (x '(red blue green) 'flowers)
  (format t "~&Roses are ~S." x))

(defun find-first-odd (list-of-numbers)
  (dolist (e list-of-numbers)
    (format t "~&Testing ~S..." e)
    (when (oddp e)
      (format t "found and odd number.")
      (return e))))

(find-first-odd '(2 4 6 8 10))

(find-first-odd '(2 4 6 7 8))

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))

(check-all-odd '(1 3 5))

(check-all-odd '(1 3 4 5))

;;; 11.1
(defun it-member (x lst)
  (dolist (e lst nil)
    (if (equal x e)
        (return t))))

;;; 11.2
(defun it-assoc (x lst)
  (dolist (e lst nil)
    (if (equal x (first e))
        (return e))))

;;; COMPARING RECURSIVE AND ITERATIVE SEARCH
(defun rec-ffo (x)
  "Recursively find first odd number in a list."
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (rec-ffo (rest x)))))


(defun it-ffo (list-of-numbers)
  "Iteratively find first odd number in a list."
  (dolist (e list-of-numbers)
    (if (oddp e) (return e))))

(defun in-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ 1 i))))))

(in-fact 5)

(defun it-intersection (x y)
  (let ((result-set nil))
    (dolist (e x result-set)
      (if (member e y)
          (push e result-set)))))

;;; 11.4
(defun it-length (x)
  (let ((result 0))
    (dolist (e x result)
      (setf result (1+ result)))))

;;; 11.5
(defun it-nth (n lst)
  (let ((i 0))
    (dolist (element lst nil)
      (if (= i n)
          (return element)
          (incf i)))))

;;; 11.6
(defun it-union (x y)
  (let ((result nil))
    (dolist (e x (append result y))
      (unless (member e y)
        (push e result)))))

;;; COMPARING DOLIST WITH MAPCAR AND RECURSION
(defun app-square-list (list-of-numbers)
  (mapcar #'(lambda (n) (* n n))
          list-of-numbers))

(app-square-list '(1 2 3 4 5))

(defun rec-square-list (x)
  (cond ((null x) nil)
        (t (cons (* (first x) (first x))
                 (rec-square-list (rest x))))))

(defun it-square-list (list-of-numbers)
  (let ((result nil))
    (dolist (e list-of-numbers (reverse result))
      (push (* e e) result))))

;;; 11.7
(defun it-reverse (lst)
  (let ((result nil))
    (dolist (e lst result)
      (push e result))))

;;; THE DO MACRO
(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt)
       (format t "Blast off!"))
    (format t "~S..." cnt)))

(launch 10)

;;; 11.9
(defun check-all-odd (list-of-numbers)
  (do ((e list-of-numbers (rest e)))
      ((evenp (first e)) nil)))

;;; 11.10
(defun it-launch (n)
  (dotimes (i n (format t "Blast off!"))
    (format t "~S..." (- n i))))

(defun count-slices (loaf)
  (do ((cnt 0 (1+ cnt))
       (z loaf (rest z)))
      ((null z) cnt)))

;;; ADVANTAGES OF IMPLICIT ASSIGNMENT
(defun fact (n)
  (do ((i n (1- i))
       (result 1 (* result i)))
      ((zerop i) result)))

(defun fact (n)
  (do ((i n (1- i))
       (result 1))
      ((zerop i) result)
    (setf result (* result i))))

(defun it-intersection (x y)
  (do ((e x (rest e))
       (result nil))
      ((null e) result)
    (if (member (first e) y)
        (push (first e) result))))

(defun find-matching-elements (x y)
  "Search X and Y for elements that match."
  (do ((x1 x (rest x1))
       (y1 y (rest y1)))
      ((or (null x1) (null y1)) nil)
    (if (equal (first x1)
               (first y1))
        (return (first x1)))))

(find-matching-elements
 '(b i r d)
 '(c a r p e t))

;;; find-first-odd with written with DO
(defun ffo-with-do (list-of-numbers)
  (do ((numbers list-of-numbers (rest numbers)))
      ((null numbers) nil)
    (if (oddp (first numbers))
        (return (first numbers)))))

(defun ffo-with-do* (list-of-numbers)
  (do* ((numbers list-of-numbers (rest numbers))
        (e (first numbers) (first numbers)))
       ((null numbers) nil)
    (if (oddp e) (return e))))

;;; 11.11
;;; DOLIST version
(defun find-largest (list-of-numbers)
  (let ((largest (first list-of-numbers)))
    (dolist (number (rest list-of-numbers) largest)
      (if (> number largest)
          (setf largest number)))))

;;; DO* version
(defun find-largest (list-of-numbers)
  (do* ((largest (first list-of-numbers))
        (numbers (rest list-of-numbers) (rest numbers))
        (number (first numbers) (first numbers)))
       ((null numbers) largest)
    (when (> number largest)
      (setf largest number))))

;;; 11.12
;;; DOTIMES version
(defun power-of-2 (n)
  (let ((result 1))
    (dotimes (i n result)
      (incf result result))))

(defun power-of-2 (n)
  (do ((result 1 (+ result result))
       (i n (1- i)))
      ((zerop i) result)))

;;; 11.13
;;; DO* version
(defun first-non-integer (x)
  "Return the first non-integer element of X."
  (do* ((z x (rest z))
        (z1 (first z) (first z)))
       ((null z) 'none)
    (unless (integerp z1)
      (return z1))))

;;; DOLIST version
(defun first-non-integer (x)
  (dolist (e x 'none)
    (unless (integerp e)
      (return e))))

;;; INFINITE LOOPS WITH DO
(defun read-a-number ()
  (do ((answer nil))
      (nil)
    (format t "~&Please type a number: ")
    (setf answer (read))
    (if (numberp answer)
        (return answer))
    (format t
            "~&Sorry, ~S is not a number. Try again."
            answer)))

;;; IMPLICIT BLOCKS
(defun find-first-odd (x)
  (format t "~&Searching for an odd number...")
  (dolist (element x)
    (when (oddp element)
      (format t "~&Found ~S." element)
      (return-from find-first-odd element)))
  (format t "~&None found.")
  'None)

(defun square-list (x)
  (mapcar
   #'(lambda (e)
       (if (numberp e)
           (* e e)
           (return-from square-list 'nope)))
   x))

;;; 11.18
(dotimes (i 5 i)
  (format t "~&I = ~S" i))

(do ((i 0 (1+ i)))
    ((= i 5) i)
  (format t "~&I = ~S" i))

;;; 11.21
(defun it-fib (n)
  (do ((i 1 (1+ i))
        (curr 1 (+ curr prev))
        (prev 1 curr))
       ((>= i n) curr)))

;;; Keyboard Exercise
;;; 11.22
(defun complement-base (base)
  (second
   (assoc base '((a t) (t a) (g c) (c g)))))

(defun complement-strand (strand)
  (mapcar #'complement-base strand))

(defun complement-strand (strand)
  (do ((s strand (rest s))
       (result nil
               (cons (complement-base (first s))
                     result)))
      ((null s) (reverse result))))

(defun make-double (strand)
  (mapcar #'list strand (complement-strand strand)))

(defun make-double (strand)
  (do ((s strand (rest s))
       (result nil
               (cons (list (first s)
                           (complement-base (first s)))
                     result)))
      ((null s) (reverse result))))

(defun count-bases (dna)
  (let ((acnt 0) (tcnt 0) (gcnt 0) (ccnt 0))
    (labels ((count-one-base (base)
                            (cond ((equal base 'a) (incf acnt))
                                  ((equal base 't) (incf tcnt))
                                  ((equal base 'g) (incf gcnt))
                                  ((equal base 'c) (incf ccnt)))))
           (dolist (element dna)
             (cond ((atom element) (count-one-base element))
                   (t (count-one-base (first element))
                      (count-one-base (second element)))))
           (list (list 'a acnt)
                 (list 't tcnt)
                 (list 'g gcnt)
                 (list 'c ccnt)))))

(defun prefixp (strand1 strand2)
  (do ((e1 strand1 (rest e1))
       (e2 strand2 (rest e2)))
      ((or (null e1) (null e2))
       (null e1))
    (when (not (equal (first e1) (first e2)))
      (return nil))))

(defun appearsp (strand1 strand2)
  (do ((e2 strand2 (rest e2)))
      ((null e2) nil)
    (if (prefixp strand1 e2)
        (return t))))

(defun coverp (strand1 strand2)
  (do* ((len1 (length strand1))
        (s2 strand2 (nthcdr len1 s2)))
       ((null s2) t)
    (when (not (prefixp strand1 s2))
      (return nil))))

(defun prefix (n strand)
  (do ((i 0 (1+ i))
       (result nil))
      ((= i n) (reverse result))
    (push (nth i strand) result)))

(defun kernel (strand)
  (do* ((cnt 1 (1+ cnt))
       (len (length strand))
       (result (prefix cnt strand) (prefix cnt strand)))
      ((> cnt len) nil)
    (if (coverp result strand)
        (return result))))

(defun draw-dna (strand)
  (let ((n (length strand)))
    (draw-string n "-----")
    (draw-string n "  !  ")
    (draw-bases strand)
    (draw-string n "  .  ")
    (draw-string n "  .  ")
    (draw-bases (complement-strand strand))
    (draw-string n "  !  ")
    (draw-string n "-----")))

(defun draw-string (cnt string)
  (format t "~&")
  (dotimes (_ cnt)
    (format t "~A" string)))

(defun draw-bases (strand)
  (format t "~&")
  (dolist (base strand)
    (format t "  ~A  " base)))

;;; Lisp Toolkit: TIME
(defun addup (n)
  "Adds up the first N integers"
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((> i n) sum)))

(time (addup 1000))

;;; Advanced Topics
(prog1
    (frist x)
  (setf (rest x)))

(let ((old-top (first x)))
  (setf x (rest x))
  old-top)

;;; OPTIONAL ARGUMENTS
(defun foo (x &optional y)
  (format t "~&X is ~S" x)
  (format t "~&Y is ~S" y)
  (list x y))

(defun divide-check (dividend &optional (divisor 2))
  (format t "~&~S ~A divide evenly by ~S"
          dividend
          (if (zerop (rem dividend divisor))
              "does"
              "does not")
          divisor))

(divide-check 27 3)
(divide-check 27)

;;; REST ARGUMENTS
(defun average (&rest args)
  (/ (reduce #'+ args)
     (length args)
     1.0))

(average 1 2 3 4 5)

(average 3 5 11 19)

;;; KEYWORD ARGUMENTS
(defun make-sundae (name &key (size 'regular)
                           (ice-cream 'vanilla)
                           (syrup 'hot-fudge)
                           nuts
                           cherries
                           whipped-cream)
  (list 'sundae
        (list 'for name)
        (list ice-cream 'with syrup 'syrup)
        (list 'toppings '=
              (remove nil
                      (list (and nuts 'nugs)
                            (and cherries 'cherries)
                            (and whipped-cream
                                 'whipped-cream))))))

(make-sundae 'cindy
             :syrup 'strawberry
             :nuts t
             :cherries t)

;;; Auxiliary variables
(defun average (&rest args
                &aux (len (length args)))
  (/ (reduce #'+ args)
     len 1.0))

;;; Quick swap the value of two variable
(let ((a 1)
      (b 2))
  (rotatef a b)
  (cons a b))
