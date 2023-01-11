(defparameter *board* (list (list nil nil nil)
                            (list nil nil nil)
                            (list nil nil nil)))

(defun display-row (row)
  (format t "~{~:[ ~:;~:*~A~] ~^| ~}~%" row))

(defun display-board (board)
  (mapc #'display-row board))

(defun all-player-p (row player)
  (every (lambda (ele)
           (string-equal ele player))
         row))

(defun vertical-row (n board)
  (loop for row in board
        collect (elt row n)))

(defun vertical-rows (board)
  (mapcar (lambda (n)
            (vertical-row n board))
          '(0 1 2)))

(defun diagonol-rows (board)
  (list
   (let ((n -1))
     (loop for row in board
           collect (elt row (incf n))))
   (let ((n 3))
     (loop for row in board
           collect (elt row (decf n))))))

(defun %win-condition-row (row player)
  (some #'identity (mapcar (lambda (row)
                             (all-player-p row player))
                           rows)))

(defun win-condition (board player)
  (or (%win-condition-row board player)
      (%win-condition-row (diagonol-rows board) player)
      (%win-condition-row (vertical-row board) player)))

(defmacro destructure-pos ((pos) &body body)
  "Destructures list POS and provides two variables X and Y within BODY."
  `(destructuring-bind (x y)
      ,pos
    (locally ,@body)))

(defun get-board-position (board pos)
  (destructure-pos (pos)
    (elt (elt board y) x)))

;;; pos (x y)
(defun add-user-move (board pos player)
  (destructure-pos (pos)
    (setf (elt (elt board y) x) player)) )

(defun place-token-p (board pos)
  (get-board-position board pos))

(defun validate-user-input (pos)
  (handler-case
      ((let ((pos (mapcar #'parse-integer pos)))
         (and (= (length pos) 2)
              (destructure-pos (pos)
                (and (<= 0 x 2)
                     (<= 0 y 2)))
              pos))
       (condition ()
                  nil))))


(defun opposite-player (player)
  (if (string-equal player "X")
      "O"
      "X"))
