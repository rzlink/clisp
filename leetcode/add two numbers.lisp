(defun add-two-numbers (lst1 lst2)
  (let ((result '(0)))
       (labels ((traverse (lst1 lst2 sum carry)
                  (if (and (null lst1) (null lst2) (= 0 carry))
                      (cdr result)
                      (let ((value carry))
                        (incf value (if (null lst1) 0 (car lst1)))
                        (incf value (if (null lst2) 0 (car lst2)))
                        (setq carry (floor value 10))
                        (setf (cdr sum) (list (mod value 10)))
                        (traverse (if (null lst1) lst1 (cdr lst1))
                                  (if (null lst2) lst2 (cdr lst2))
                                  (cdr sum)
                                  carry)))))
         (traverse lst1 lst2 result 0))))

(add-two-numbers '(2 3 4) '(5 6 4))
