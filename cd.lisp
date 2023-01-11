(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun make-cd (title artist rating ripped)
  `(:title ,title :artist ,artist :rating ,artist :ripped ,ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: "))
            (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (file-name)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))


(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title) title) t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row)
         *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defmacro backwards (expr)
  (reverse expr))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collect (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@ (make-comparisons-list clauses))))

`(and ,(list 1 2 3))
`(and ,@ (list 1 2 3))
(where :title "Give Us a Break" :ripped t)

(select (where :title "Fly" :ripped t))

;;; (foo 1 2 3) == (funcall '#foo 1 2 3)

(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(setf x 10 y 20)
(rotatef x y)

(defun foo (x)
  (cond
    ((> x 5) (print "X is great then 5") 5)
    ((> x 3) (print "x is 4 or 5") 3)
    (t (print "others") 100)))
