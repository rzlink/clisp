(setf *total-glasses* 0)

(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* n))
  (format t
          "~&That make ~S glasses so far today."
          *total-glasses*))

;;; 10.1
(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (incf *total-glasses* n))

;;; 10.3.2 The PUSH and POP Macros
