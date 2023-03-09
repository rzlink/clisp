(defvar my-vec '#(runing violin 440 a))

(aref my-vec 1)

(defparameter a '#(nil nil nil nil nil ))
(setf (aref a 0) 'foo)

(setf (aref a 1) 37)

(make-array 5 :initial-element 1)
(defvar b (make-array 5 :initial-contents '(a e i o u)))

(reverse b)

(aref "Cockatoo" 3)

(type-of #\k)
(defvar pet "Cockatoo")
(setf (aref pet 5) #\p)

(defvar h (make-hash-table))
(type-of h)

(setf (gethash 'john h)
      '(attorney (16 maple drive)))

(setf (gethash 'mary h)
      '(physician (23 cedar court)))

(describe h)

;;; 13.8 PROPERTY LISTS
(setf (get 'fred 'sex) 'male)
(setf (get 'fred 'age) '17)

(describe 'fred)
(incf (get 'fred 'age))

;;; Third parameter return instead of 'nil'
(setf (get 'mabel 'siblings) nil)
(get 'mabel 'siblings 'unknown)
(get 'clare 'siblings 'unknown)

;;; Remove a property
(remprop 'mabel 'siblings)
(get 'mabel 'siblings)

(defun my-addprop (sym elem prop)
  (pushnew elem (get sym prop)))

(defun record-meeting (x y)
  (my-addprop x y 'has-met)
  (my-addprop y x 'has-met))

(symbol-plist 'little-red)
(record-meeting 'little-red 'wolfie)
(record-meeting 'wolfie 'grandma)

(symbol-plist 'little-red)
(symbol-plist 'wolfie)

;;; 13.1
(defun subprop (symbol item property)
  (setf (get symbol property)
        (remove item (get symbol property))))

;;; 13.2
(defun forget-meeting (person1 person2)
  (subprop person1 person2 'has-met)
  (subprop person2 person1 'has-met)
  'forgotten)

;;; 13.3
(defun my-get (symbol prop)
  (labels ((rec (props)
             (cond ((null props) nil)
                   ((equal (first props) prop) (second props))
                   (t (rec (cddr props))))))
    (rec (symbol-plist symbol))))

(defun my-get (symbol property)
  (do ((properties (symbol-plist symbol) (cddr properties)))
      ((null properties) nil)
    (when (eql (first properties) property)
      (return (second properties)))))

;;; 13.4
(defun hasprop (symbol property)
  (do ((properties (symbol-plist symbol) (cddr properties)))
      ((null properties) nil)
    (when (equal (first properties) property)
      (return T))))

;;; Keyboard Exercise
;;; 13.8
(defparameter *hist-array* nil)
(defparameter *total-points* 0)

(defun new-histogram (n)
  (setf *total-points* 0)
  (setf *hist-array*
        (make-array n :initial-element 0))
  t)

(defun record-value (n)
  (cond ((>= n (length *hist-array*))
         (error "Value ~S out of bounds." n))
        (t (incf (aref *hist-array* n))
           (incf *total-points*))))

(defun print-hist-line (n)
  (let ((count (aref *hist-array* n)))
    (format t "~&~2S [~3S] " n count)
    (dotimes (_ count)
      (format t "*"))))

(defun print-histogram ()
  (dotimes (i (length *hist-array*))
    (print-hist-line i))
  (format t "~&~7S total" *total-points*))


;;; Hash Table Keyboard Exercise
;;; 13.9
(defparameter crypto-text
  '("zj ze kljjls jf slapzi ezvlij pib kl jufwxuj p hffv jupi jf"
    "enlpo pib slafml pvv bfwkj"))

(defparameter *encipher-table* (make-hash-table))
(defparameter *decipher-table* (make-hash-table))

(defun make-substitution (code clear)
  (setf (gethash code *encipher-table*) clear)
  (setf (gethash clear *decipher-table*) code))

(defun undo-substitution (code clear)
  (setf (gethash code *encipher-table*) nil)
  (setf (gethash clear *decipher-table*) nil))

(defun clear ()
  (clrhash *encipher-table*)
  (clrhash *decipher-table*))

(defun decipher-string (string)
  (do* ((len (length string))
        (new-string (make-string len :initial-element #\Space))
        (i 0 (1+ i)))
       ((equal i len) new-string)
    (let* ((char (aref string i))
           (new-char (gethash char *decipher-table*)))
      (when new-char
        (setf (aref new-string i) new-char)))))

(defun show-line (line)
  (format t "~%~A~%~A~%"
          line
          (decipher-string line)))

(defun show-text ()
  (format t "~&--------------------")
  (dolist (line crypto-text)
    (show-line line))
  (format t "~&--------------------"))

(defun get-first-char (x)
  (char-downcase
   (char (format nil "~A" x) 0)))

(defun read-letter ()
  (let ((obj (read)))
    (if (member obj '(end undo))
        obj
        (get-first-char obj))))

(defun sub-letter (code)
  (when (gethash code *decipher-table*)
    (format t "~&'~A' has already been" code)
    (format t " deciphered as '~A'!"
            (gethash code *decipher-table*))
    (return-from sub-letter nil))
  (format t "What does '~A' decipher to? " code)
  (let ((clear (read-letter)))
    (cond ((not (characterp clear))
           (format t "~&Invalid response."))
          ((gethash clear *encipher-table*)
           (format t "But '~A' already deciphers as '~A'!"
                   (gethash clear *encipher-table*)
                   clear))
          (t (make-substitution code clear)))))

(defun undo-letter ()
  (format t "~&Undo which letter? ")
  (let* ((code (read-letter))
         (clear (gethash code
                         *decipher-table*)))
    (cond ((not (characterp code))
           (format t "~&Invalid input."))
          (clear (undo-substitution code clear))
          (t (format t "~&But '~A' wasn't deciphered!"
                     code)))))

(defun solve ()
  (do ((resp nil))
      ((equal resp 'end))
    (format t "~&Substitute which letter? ")
    (setf resp (read-letter))
    (cond ((characterp resp) (sub-letter resp))
          ((equal resp 'undo) (undo-letter))
          ((equal resp 'end) nil)
          (t (format t "~&Invalid input.")))))

(make-array 3 :element-type 'string-char
            :initial-contents '(#\M #\o #\m))
