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

(pathname-directory "home/rzlink/github/clisp/test.txt") ; -> (:RELATIVE "home" "rzlink" "github" "clisp")

(directory-namestring #p"home/rzlink/github/clisp/test.txt") ; -> "home/rzlink/github/clisp/"

(file-namestring #"/home/rzlink/github/clisp/test.txt") ; -> "test.txt"

;;; constructing new pathnames
(make-pathname
 :directory '(:absolute "foo" "bar")
 :name "baz"
 :type "txt")                           ; #P"/foo/bar/baz.txt"

(make-pathname :type "html" :defaults input-file)

(merge-pathnames #p "foo/bar.html" #p "/WWW/html/") ; ->  #P"/www/html/foo/bar.html"

(enough-namestring #p "/www/html/foo/bar.html" #p "/www/") ; -> "html/foo/bar.html"

;;; if merge-pathnames only provide single argument, the second argument will use *default-pathname-defaults*
(setf *default-pathname-defaults* #p "/home/rzlink/")
(merge-pathnames #p "foo.txt")          ; -> #P"/home/rzlink/foo.txt"

;;; Get the length of a file
(with-open-file (in "/home/rzlink/github/clisp/files_and_fileio.lisp" :element-type '(unsigned-byte 8))
  (file-length in))

(let ((s (make-string-input-stream "1.23")))
  (unwind-protect (read s)
    (close s)))

(with-output-to-string (out)
  (format out "hello, world ")
  (format out "~s" (list 1 2 3)))       ; -> "hello, world (1 2 3)"
