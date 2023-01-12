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

(member 'b '(a b c))                    ; -> (b c)
(member 'd '(a b c))                    ; -> nil
(find 'b '(a b c))                      ; -> b
(find 'd '(a b c))                      ; -> nil

;;; intersection returns a list containing all the elements found in
;;; both arguments.
(intersection '(a b) '(a b c d))        ; -> (a b)

;;; union returns a list containing one instance of each unique
;;; element from the two arguments.
(union '(a b b) '(b c d))               ; -> (a b c d)

;;; set-difference returns a list containing all the elements from
;;; the first argument that don't appear in the second argument.
(set-difference '(a b) '(a c))          ; -> (b)

(subsetp '(3 2 1) '(1 2 3 4))           ; -> T
(subsetp '(1 2 3 4) '(3 2 1))           ; -> nil

;;; Lookup tables: Alists and Plists
(assoc 'a '((a . 1) (b . 2) (c . 3)))   ; -> (a . 1)
(assoc 'c '((a . 1) (b . 2) (c . 3)))   ; -> (c . 3)
(assoc 'd '((a . 1) (b . 2) (c . 3)))   ; -> nil

;;; get the value use the cdr
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3)))) ; -> 1
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=) ; -> ("a" . 1)

;;; you can add a pair to the front of an alist with CONS:
(defvar *alist* '((a . 1) (b . 2) (c . 3)))
(cons (cons 'd 4) *alist*)              ; -> ((D . 4) (A . 1) (B . 2) (C . 3))

;;; you can also use acons function
(acons 'd 4 *alist*)

;;; to modify an alist
(setf *alist* (cons (cons 'd 4) *alist*) )
;;; or use the push
(push (cons 'e 5) *alist*)

;;; function pairlis can build an alist from two seperate lists
(pairlis '(a b c) '(1 2 3))             ; -> ((C . 3) (B . 2) (A . 1))

;;; plist is like (a 1 b 2 c 3)
;;; you can use setf with getf to add a new key/value pair
(defparameter *plist* ())
(setf (getf *plist* :a) 1)              ; -> (:a 1)
(getf *plist* :a)                       ; -> 1

;;; to remove a key/value pair from a plist, use remf
(remf *plist* :a)                       ; -> T
