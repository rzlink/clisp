(defun mary ()
  (format t "~&Mary had a little bat.")
  (format t "~&Its wings were long and brown.")
  (format t "~&And everywhere that Mary went")
  (format t "~&The bat went, upside-down."))


;;; ~S inserts the printed representation of a lisp object
(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))

(format t "From ~S to ~S in ~S minutes"
        'boston '(new york) 55)

(mapcar #'square-talk '(1 2 3 4 5))

;;; ~A directive prints an object without using escape characters
(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Without escape characters: ~A" x))

(test "hello")

;;; 9.1
(defun saying ()
  (format t "~&There are old pilots,")
  (format t "~&and there are bold pilots,")
  (format t "~&but there are no old bold pilots."))
(saying)

;;; 9.2
(defun draw-line (n)
  (cond ((zerop n) (format t "~&"))
        (t (format t "*")
           (draw-line (1- n)))))
(draw-line 10)

;;; 9.3
(defun draw-box (x y)
  (cond ((zerop y) nil)
        (t
         (draw-line x)
         (draw-box x (1- y)))))

(draw-box 10 4)

;;; 9.4
(defun ninety-nine-bottle (n)
  (format t "~&~S bottles of bear on the wall," n)
  (format t "~&~S bottles of beer!" n)
  (format t "~&Take one down,")
  (format t "~&Pass it around,")
  (format t "~&~S bottles of beer on the wall." (1- n)))

(defun ninety-nine-bottles (n)
  (cond ((zerop n) nil)
        (t (ninety-nine-bottle n)
           (ninety-nine-bottles (1- n)))))

(ninety-nine-bottles 99)

;;; 9,5
(defun print-board (board)
  (let ((b2 (sublis '((x . "X")
                          (o . "O")
                          (nil . " "))
                        board)))
    (format t "~&")
    (print-line b2)
    (format t "-----------~%")
    (print-line (nthcdr 3 b2))
    (format t "-----------~%")
    (print-line (nthcdr 6 b2))))

(defun print-line (line)
  (format t " ~A | ~A | ~A~%"
          (first line)
          (second line)
          (third line)))

;;; THE READ FUNCTION
(defun my-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The number ~S squared is ~S.~%"
            x (* x x))))

;;; 9.6
(defun compute-pay ()
  (format t "~&What is the hourly wage? ")
  (let ((wage (read)))
    (format t "~&How many hours worked? ")
    (let ((hours (read)))
      (format t "~&The worker earned ~S dollars."
              (* wage hours)))))

;;; The YES-OR-NO-P FUNCTION
(defun riddle ()
  (if (yes-or-no-p
       "Do you seek Zen enlightenment? ")
      (format t "Then do not ask for it!")
      (format t "You have found it.")))

;;; READING FILES WITH WITH-OPEN-FILE
(defun get-tree-data ()
  (with-open-file (stream "~/github/clisp/timber.dat")
    (let* ((tree-loc (read stream))
           (tree-table (read stream))
           (num-trees (read stream)))
      (format t "~&There are ~S trees on ~S."
              num-trees tree-loc)
      (format t "~&The are: ~S" tree-table))))

;;; WRITING FILES WITH WITH-OPEN-FILE
(defun save-tree-date (tree-loc tree-table num-trees)
  (with-open-file (stream "~/github/clisp/timber.dat"
                          :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" num-trees)))

;;; Keyboard Exercise
;;; 9.10
(defun space-over (n)
  (cond ((plusp n)
         (format t " ")
         (space-over (1- n)))
        ((zerop n) nil)
        (t (format t "Error!"))))

(defun test (n)
  (format t "~%>>>")
  (space-over n)
  (format t "<<<<"))

(defun plot-one-point (plotting-string y-val)
  (space-over y-val)
  (format t "~A~%" plotting-string))

(defun plot-points (s lst)
  (mapcar #'(lambda (x)
              (plot-one-point s x))
          lst))

(defun generate (m n)
  (cond ((> m n) nil)
        ((equal m n) (list m))
        (t (cons m (generate (1+ m) n)))))

(defun make-graph ()
  (let ((func (prompt-for "Function to graph? "))
        (start (prompt-for "Starting x value? "))
        (end (prompt-for "Ending x value? "))
         (plotting-string (prompt-for "Plotting string? ")))
    (plot-points plotting-string
                 (mapcar func (generate start end)))
    t))

(defun prompt-for (prompt-string)
  (format t "~A" prompt-string)
  (read))

(defun square (n)
  (* n n))

(setf glee-club
      '((john smilth) (barbara wilson) (mustapha ali)))

(defun print-one-name (name)
  (format t "~&~10S ~S"
          (second name)
          (first name)))

(defun print-all-names (x)
  (mapcar #'print-one-name x)
  'done)

(defun sevenths (x)
  (mapcar #'(lambda (numerator)
            (format t "~&~4,2F / 7 is ~7,5F"
             numerator
             (/ numerator 7.0)))
          x)
  'done)

(sevenths '(1 3/2 2 2.5 3))

;;; HANDING END-OF-FILE CONDITIONS
(defun read-my-file ()
  (with-open-file (stream "~/github/clisp/timber.dat")
    (let ((contents
            (read-all-objects stream (list '$eof$))))
      (format t "~&Read ~S objects from the file."
              (length contents))
      contents)))

(defun read-all-objects (stream eof-indicator)
  (let ((result (read stream nil eof-indicator)))
    (if (eq result eof-indicator)
        nil
        (cons result (read-all-objects stream)))))

;;; PRINT IN DOT NOTATION
;;; 9.11
(defun dot-prin1 (lst)
  (cond ((atom lst) (format t "~S" lst))
        (t
         (format t "(")
         (dot-prin1 (first lst))
         (format t " . ")
         (dot-prin1 (rest lst))
         (format t ")"))))

;;; 9.15
(defun hybrid-prin1)
