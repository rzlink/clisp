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
