(defun two-sum (lst target)
  (let ((h (make-hash-table)))
    (labels ((f (lst i)
                (unless (null lst)
                  (let* ((x (car lst))
                         (key (- target x))
                         (value (gethash key h)))
                    (if value
                        (values i value)
                        (progn
                          (setf (gethash x h) i)
                          (f (cdr lst) (1+ i))))))))
      (f lst 0))))

(defparameter *x* '(2 7 11 15))
(defparameter *target* 9)
(two-sum *x* *target*)


(defun two-sum (nums target)
  (let ((hash-table (make-hash-table :test #'equal)))
    (loop for i from 0 below (length nums) do
      (let ((complement (- target (elt nums i))))
        (when (gethash complement hash-table)
          (return (list (gethash complement hash-table) i))))
      (setf (gethash (elt nums i) hash-table i))
            nil)))
