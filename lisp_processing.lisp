(cons 1 2)                              ; -> (1 . 2)
(car (cons 1 2))                        ; -> 1
(cdr (cons 1 2))                        ; -> 2

;;; you can use SET with CAR, CDR to set the value
(defparameter *cons* (cons 1 2))

(setf (car *cons*) 10)                  ; -> (10 . 2)
(setf (cdr *cons*) 20)                  ; -> (10. 20)

(cons 1 nil)                            ; -> (1)
(cons 1 (cons 2 nil))                   ; -> (1 2)
(cons 1 (cons 2 (cons 3 nil)))          ; -> (1 2 3)

(list 1 2 3)                            ; -> (1 2 3)

;;; you can also use first and rest for CAR and CDR
(defparameter *list* (list 1 2 3 4))
(first *list*)                          ; -> 1
(rest *list*)                           ; -> (2 3 4)
(first (rest *list*))                   ; -> 2

;;; different types
(list "foo" (list 1 2) 10)              ; -> ("foo" (1 2) 10)
(first (rest '("foo" (1 2) 10)))        ; -> (1 2)
