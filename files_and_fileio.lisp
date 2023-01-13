(let ((in (open "/home/rzlink/github/clisp/files_and_fileio.lisp")))
  (format t "~a~%" (read-line in))
  (close in))

(let ((in (open "/home/rzlink/github/clisp/test.lisp" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

;;; Functions: read-char, read-line, read, all take an optional argument,
;;; which default to true, whether they should signal an error if they're
;;; called at the end of the file. If set to nil, it will return nil.
;;; You can print all lines in a file like this:
(let ((in (open "/home/rzlink/github/clisp/files_and_fileio.lisp" :if-does-not-exist :error)))
  (loop for line = (read-line in nil)
        while line do (format t "~a~%" line))
  (close in))

;;; read will read a single s-expression
(let ((in (open "/home/rzlink/github/clisp/files_and_fileio.lisp")))
  (format t "~a~%" (read in))
  (close in))

;;; Read binary data
;;; by default OPEN function returns characters stream.
;;; To read the raw bytes, pass OPEN an :element-type argument of '(unsigned-byte 8)
;;; Use function read-byte.

;;; Bulk read - read-sequence

(open "/home/rzlink/github/clisp/test.txt" :direction :output :if-exists :supersede)

(with-open-file (stream "/home/rzlink/github/clisp/test.text" :direction :output)
  (format stream "Some text."))
