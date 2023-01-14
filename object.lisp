(defgeneric draw (shape)
  (:documentation "Draw the given shape on the screen."))


(defgeneric withdraw (account amount)
  (:documentation "With draw the specified amount from the account.
Signal an error if the current balance is less that amount."))

(defmethod withdraw ((account bank-account) amount)
  (when (< (balance account) amount)
    (error "account over drawn."))
  (decf (balance account) amount))

(defmethod withdraw ((account checking-account) amount)
  (let ((overdraft (- amound (balance account))))
    (when (plusp overdraft)
      (withdraw (overdraft-account account) overdraft)
      (incf (balance account) overdraft)))
  (call-next-method))

(defclass person ()
  ((name
    :initarg :name
    :accessor name)
   (lisper
    :initform nil
    :accessor lisper)))

(defvar p1 (make-instance 'person :name "me"))

(name p1)                               ; -> me
(lisper p1)                             ; -> nil

(defclass child (person)
  ())

(defclass child (person)
  ((can-walk-p
    :accessor can-walk-p
    :initform t)))

(can-walk-p (make-instance 'child))     ; -> T

(defclass bank-account ()
  (customer-name
   balance))

(defparameter *account* (make-instance 'bank-account))
(print-object *account* t)
(setf (slot-value *account* 'customer-name) "John Doe")
(setf (slot-value *account* 'balance) 1000)

(slot-value *account* 'customer-name)   ; -> "John Doe"
(slot-value *account* 'balance)         ; -> 1000
