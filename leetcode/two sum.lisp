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
