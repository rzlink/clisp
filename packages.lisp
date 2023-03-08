(in-package :cl-user)

(defpackage :package-1
  (:documentation "First package document string")
  (:use :common-lisp)
  (:nicknames :package1)
  (:export
   :hello-wold
   :mult
   :div))

(in-package :package1)

(defun hello-world ()
  (format t "Hello from package1!~%"))

(defun mult (a b)
  (format t "calling package1's div operation~%")
  (* a b))

(defun div (a b)
  (format t "Calling package1's div operation~%")
  (/ a b))

(defpackage :package-2
  (:documentation "Another package")
  (:nicknames :package2)
  (:use :cl)
  (:export
   :hello-wold
   :add
   :sub))

(in-package :package2)

(defun hello-word ()
  (format t "Hello from package2!~%"))

(defun add (a b)
  (+ a b))

(defun sub (a b)
  (format t "Calling package2's sub function")
  (- a b))

(defpackage :package-3
  (:nicknames :package3)
  (:use :cl :package2)
  (:import-from :package1 :mult)
  (:shadowing-import-from :package1 :hello-world)
  (:shadow :sub))

(in-package :package3)

(defun another-function ()
  (format t "Calling utils:hello-world from another-function~%")
  (hello-world))

(defun sub (a b)
  (format t "calling package3's sub function")
  (- a b))
