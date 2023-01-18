(in-package :com.gigamonkeys.pathnames)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))
