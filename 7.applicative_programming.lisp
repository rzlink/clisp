(defun add1 (x)
  (+ 1 x))

;;; 7.5
(lambda (x) (- x 7))

;;; 7.6
(lambda (x)
  (or (null x) (equal x t)))

;;; 7.7
(mapcar #'(lambda (x)
            (cond ((equal x 'up) 'down)
                  ((equal x 'down) 'up)
                  (t (error "invalid input"))))
        '(up down up up))

(defun my-assoc (key table)
  (find-if #'(lambda (entry)
               (equal (first entry) key))
           table))

;;; 7.8
(defun roughly-equal (e k)
  (and (>= e (- k 10))
       (<= e (+ k 10))))

(defun find-first-roughly-equal (x k)
  (find-if #'(lambda (e) (roughly-equal e k))
           x))

;;; 7.9
(defun find-nested (x)
  (find-if #'consp x))

;;; 7.10
(defvar *note-table*
  '((c 1)
    (c-sharp 2)
    (d 3)
    (d-sharp 4)
    (e 5)
    (f 6)
    (f-sharp 7)
    (g 8)
    (g-sharp 9)
    (a 10)
    (a-sharp 11)
    (b 12)))

(defun numbers (notes)
  (mapcar #'(lambda (note)
              (second (assoc note *note-table*)))
          notes))

(defun number-to-note (number)
  (first (find-if #'(lambda (e)
                (equal number (second e)))
            *note-table*)))

(defun notes (numbers)
  (mapcar #'number-to-note numbers))

(defun raise (n x)
  (mapcar #'(lambda (x) (+ n x))
          x))

(defun normalize (x)
  (mapcar #'(lambda (x)
              (cond ((< x 1) (+ x 12))
                    ((> x 12) (- x 12))
                    (t x)))
          x))

(defun transpose (n song)
  (notes (normalize (raise n (numbers song)))))

;;; 7.11
(defun pick (x)
    (remove-if-not #'(lambda (x)
                       (and (> x 1) (< x 5)))
                   x))

;;; 7.12
(defun count-the (x)
  (length (remove-if-not #'(lambda (x) (equal x 'the))
                         x)))

;;; 7.13
(defun pick-pairs (x)
  (remove-if-not #'(lambda (x) (equal 2 (length x)))
                 x))

;;; 7.14
(defun my-setdiff (x y)
  (remove-if #'(lambda (e) (member e y)) x))

(defun my-intersection (x y)
  (remove-if-not #'(lambda (e) (member e y)) x))

(defun my-union (x y)
  (append x (remove-if #'(lambda (e)
                           (member e x))
                       y)))

;;; 7.15
(defun rank (card) (first card))

(defun suit (card) (second card))

(defvar *my-hand*
  '((3 hearts)
    (5 clubs)
    (2 diamonds)
    (4 diamonds)
    (ace spades)))

(defun count-suit (suit hand)
  (length
   (remove-if-not #'(lambda (card)
                      (equal (suit card) suit))
                  hand)))

(defvar *colors*
  '((clubs black)
    (diamonds red)
    (hearts red)
    (spades black)))

(defun color-of (card)
  (second (assoc (suit card) *colors*)))

(defun first-red (hand)
  (find-if #'(lambda (card)
               (equal 'red (color-of card)))
           hand))

(defun black-cards (hand)
  (remove-if-not #'(lambda (card)
                     (equal 'black (color-of card)))
                 hand))

(defun what-ranks (suit hand)
  (mapcar #'rank
          (remove-if-not
           #'(lambda (card)
               (equal (suit card) suit))
           hand)))

(defvar *all-ranks*
  '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun higher-rank-p (card1 card2)
  (not (member (rank card2) (member (rank card1) *all-ranks*))))

(defun high-card (hand)
  (assoc (find-if #'(lambda (card)
                (assoc card hand))
                  (reverse *all-ranks*))
         hand))

(defun high-card (hand)
  (reduce #'(lambda (card1 card2)
              (if (higher-rank-p card1 card2)
                  card1
                  card2))
          hand))

;;; 7.17
(defun total-length (x)
  (reduce #'(lambda (lst1 lst2)
              (+ (length lst1) (length lst2)))
          x))

(defun total-length (x)
  (reduce #'+ (mapcar #'length x)))

;;; 7.18 - 7.22
(defun all-odd (x)
  (every #'oddp x))

(defun none-odd (x)
  (every #'evenp x))

(defun not-all-odd (x)
  (not (all-odd x)))

(defun not-none-odd (x)
  (not (none-odd x)))

;;; 7.26
(defun my-find-if (fn lst)
  (first (remove-if-not fn lst)))

;;; 7.27
(defun my-every (fn lst)
  (null (remove-if fn lst)))

;;; keyboard exercise
;;; 7.29
(defparameter *database*
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

(defun match-element (x y)
  (or (equal x y) (equal y '?)))

(defun match-triple (x y)
  (every #'match-element x y))

(defun fetch (pattern)
  (remove-if-not #'(lambda (row)
              (match-triple row pattern))
          *database*))

(defun find-color-pattern (block)
  (list block 'color '?))

(defun supporters (block)
  (let ((pattern (list block 'supported-by '?)))
    (mapcar #'third (fetch pattern))))

(defun cubes ()
  (let ((pattern (list '? 'shape 'cube)))
    (mapcar #'first (fetch pattern))))

(defun supp-cube (block)
  (not (null (intersection (supporters block) (cubes)))))

(defun desc1 (block)
  (fetch (list block '? '?)))

(defun desc2 (block)
  (mapcar #'rest (desc1 block)))

(defun description (block)
  (reduce #'append (desc2 block)))
