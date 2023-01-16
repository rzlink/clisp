(define-condition malformed-log-entry-error (error)
  ((text :initarg :text :reader text)))

(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry)
      (error 'malformed-log-entry-error :text text)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
          for entry = (handler-case (parse-log-entry text)
                        (malformed-log-entry-error () nil))
          when entry collect it)))

(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) whil text
          for entry = (restart-case (parse-log-entry text)
                        (skip-log-entry () nil))
          when entry collect it)))


(in-package :error-hanlding)

(define-condition file-io-error (error)
  ((message :initarg :message :reader message)))

(define-condition another-file-io-error (error)
  ((message :initarg :messasge :reader message)))

(defun fake-io (&key (fail nil fail-p) (message "Nope!"))
  (cond
    ((not fail-p)
     (if (evenp (random 100))
         (error 'file-ip-error :message message)
         "Success"))

    (fail (error 'another-file-io-error :message message))
    (t "success")))
1
(defun read-new-value ()
  (format t "Enter a new value: ")
  (force-output)
  (multiple-value-list (eval (read))))

(let ((fail t))
  (restart-case (fake-io :fail fail)
    (retry-without-errors (new-fail)
      :report "Pass in a fail value"
      :interactive read-new-value
      (setf fail new-fail)
      (fake-io :fail fail))))


(fake-io)
(fake-io :fail t)
(fake-io :fail nil)


(define-condition evenp-error (error)
  ((text :initarg :text :reader text)))

(defun filter-evenp (lst)
  (dolist (x lst)
    (if (not (evenp x))
        (print x)
        (error 'evenp-error :text x))))

(defun skip-evenp (c) (invoke-start 'skip-evenp))
(restart-case (filter-evenp (list 1 2 3 4 5))
  (skip-evenp () nil))
