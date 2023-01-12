(vector)

(vector 1 2)

(make-array 5 :initial-element 1)

(make-array 5 :fill-pointer 0)

(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*)
(vector-push 'b *x*)
(vector-push 'c *x*)
(vector-pop *x*)
(vector-pop *x*)

;;; make an arbitrarily resizable vecto
(make-array 5 :fill-pointer 0 :adjustable t)

(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'bit)

(defparameter *x* (vector 1 2 3))
(length *x*)
(elt *x* 0)
(elt *x* 1)
(elt *x* 2)

;;; update the array element value
(setf (elt *x* 0) 10)

;;; sequence iterating functions
(count 1 #(1 2 1 2 3 1 2 3 4))
(remove 1 #(1 2 1 2 3 1 2 3 4))
(remove 1 '(1 2 1 2 3 1 2 3 4))
(remove #\a "foobarbaz")
(substitute 10 1 #(1 2 1 2 3 1 2 3 4))
(substitute 10 1 '(1 2 1 2 3 1 2 3 4))
(find 1 #(1 2 1 2 3 1 2 3 4))
(position 4 #(1 2 1 2 3 1 2 3 4))
(count "foo" #("foo" "bar" "baz") :test #'string=)
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t)

;;; arguments list:
;;; :test     -> two-argument function used to compare item, default: EQL
;;; :key      -> one-argument function to extract key vault from sctual sequence, NIL means use element as is, default: NIL
;;; :start    -> starting index, default: 0
;;; :end      -> ending index, default: nil
;;; :from-end -> sequence traversed in reverse order or not, default: nil
;;; count     -> number indicating the number of elements to remove or substitute, default: nil (indicate all)

;;; higher-order function variants
(count-if #'evenp #(1 2 3 4 5))

(count-if-not #'evenp #(1 2 3 4 5))

(position-if #'digit-char-p "abcd0001")

(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
               #("foo" "bar" "baz" "foom"))

(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)

(remove-if-not #'alpha-char-p #("foo" "bar" "1baz")
               :key #'(lambda (x) (elt x 0)))

(remove-duplicates #(1 2 1 2 3 1 2 3 4))

(copy-seq #(1 2 3 4 5))
(reverse #(1 2 3 4 5))

(equalp #(1 2 3 4 5) (reverse #(5 4 3 2 1)))

(concatenate 'vector #(1 2 3) '(4 5 6))
(concatenate 'list #(1 2 3) '(4 5 6))
(concatenate 'string "abc" '(#\d #\e #\f))

;;; sorting and merging
(sort (vector "foo" "bar" "baz") #'string<) ; -> #("bar" "baz" "foo")

(setf my-sequence (sort my-sequence #'string<))

(merge 'vector #(1 3 5) #(2 4 6) #'<)   ; -> #(1 2 3 4 5 6)

(merge 'list #(1 3 5) '(2 4 6) #'<)     ; -> (1 2 3 4 5 6)

;;; subsequence manipulations
(subseq "foobarbaz" 3)                  ; -> "barbaz"
(subseq "foobarbaz" 3 6)                ; -> "bar"

(defparameter *x* (copy-seq "foobarbaz"))

(setf (subseq *x* 3 6) "xxxx")          ; -> "fooxxxbaz"

(setf (subseq *x* 3 6) "ab")            ; -> "fooabxbaz"

(fill *x* #\b)                          ; -> "bbbbbbbbb"

(position #\b "foobarbaz")              ; -> 3
(search "bar" "foobarbaz")              ; -> 3

(mismatch "foobarbaz" "foom")           ; -> 3, mismatch will return nil if the strings match
(mismatch "foobar" "bar" :from-end t)   ; -> 3

;;; sequence predicates
(every #'evenp #(1 2 3 4 5))            ; -> nil
(some #'evenp #(1 2 3 4 5))             ; -> T
(notany #'evenp #(1 2 3 4 5))           ; -> nil
(notevery #'evenp #(1 2 3 4 5))         ; -> T

;;; compare elements of two sequences pairwise:
(every #'> #(1 2 3 4) #(5 4 3 2))       ; -> nil
(some #'> #(1 2 3 4) '(5 4 3 2))        ; -> T
(notany #'> #(1 2 3 4) #(5 4 3 2))      ; -> nil
(notevery #'> #(1 2 3 4) #(5 4 3 2))    ; -> T

;;; sequence mapping functions
;;; like concatenate and merge, map function need to be told what kind
;;; of sequence to create.
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6)) ; -> #(10 18 24 28 30)

;;; mapcar like map, except it always return a list.
;;; The results of each function call are collected into a new list.
(mapcar #'* '(1 2 3 4 5) '(10 9 8 7 6))      ; -> (10 18 24 28 30)
(mapcar #'(lambda (x) (* 2 x)) (list 1 2 3)) ; -> (2 4 6)

;;; maplist is like mapcar EXCEPT instead of passing the ELEMENTS of
;;; the list to the function, it passes the actual cons cell.
;;; example:
(maplist #'(lambda (x) (cons 'foo x)) '(a b c)) ; -> ((FOO A B C) (FOO B C) (FOO C))

;;; here is an example to build custom mapcar on top of maplist
(defun my-mapcar (f lst) (maplist #'(lambda (x) (funcall f (car x))) lst))
(my-mapcar #'(lambda (x) (cons 'foo x)) '(a b c)) ; -> ((FOO . A) (FOO . B) (FOO . C))

;;; reduce
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10))                   ; -> 55
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10) :initial-value 10) ; -> 65

;;; hash table
(defparameter *h* (make-hash-table))
(gethash 'foo *h*)                      ; nil, nil
(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*)                      ; quux, t

;;; gethash function return two values
;;; First value is the value stored under giving key or nil
;;; Second value indicating whether the key present in the hash table.

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
        (format nil "Value ~a actually present." value)
        (format nil "Value ~a because key not found." value))))

(setf (gethash 'bar *h*) nil)
(show-value 'foo *h*)                   ; -> "Value QUUX actually present."
(show-value 'bar *h*)                   ; -> "Value NIL actually present."
(show-value 'baz *h*)                   ; -> "Value NIL because key not found."
(remhash 'foo *h*)                      ; -> T
(remhash 'abc *h*)                      ; -> nil
(clrhash *h*)                           ; clear a hash table of all its key/value pairs

;;; hash map iteration
(setf (gethash 'foo *h*) 'quux)
(setf (gethash 'bar *h*) nil)
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
                                        ;BAR => NIL
                                        ;FOO => QUUX

(loop for k being the hash-keys in *h* using (hash-value v)
      do (format t "~a -> ~a~%" k v))

(maphash #'(lambda (k v) (when (< v 10) (remhash k *h*))) *h*)
