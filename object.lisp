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

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))

(defparameter *account* (make-instance 'bank-account :customer-name "John Doe" :balance 10000))

(slot-value *account* 'balance)         ; -> 10000
(slot-value *account* 'customer-name)   ; -> "John Doe"

(defvar *account-numbers* 0)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((> balance 100000) :gold)
            ((> balance 50000) :silver)
            (t :bronze)))))

(defmethod initialize-instance :after ((account bank-account)
                                       &key opening-bonus-percentage)
  (when opening-bonus-percentage
    (incf (slot-value account 'balance)
          (* (slot-value account 'balance) (/ opening-bonus-percentage 100)))))

(defparameter *acct* (make-instance
                      'bank-account
                      :customer-name "Sally Sue"
                      :balance 10000
                      :opening-bonus-percentage 5))

(slot-value *acct* 'balance)            ; -> 10500

(defun balance (account)
  (slot-value account 'balance))

(fmakunbound 'balance)

(defgeneric balance (account))

(defmethod balance ((account bank-account))
  (slot-value account 'balance))

(defgeneric (setf customer-name) (value account))

(defmethod (setf customer-name) (value (account bank-account))
  (setf (slot-value account 'customer-name) value))

(defgeneric customer-name (account))

(defmethod customer-name ((account bank-account))
  (slot-value account 'customer-name))

(setf (customer-name *account*) "Sally Sue")
(customer-name *account*)

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :reader customer-name
    :writer (setf customer-name))
   (balance
    :initarg :balance
    :initform 0
    :reader balance)
   (account-number
    :initform (incf *account-numbers*))
   account-type))

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name.")
    :accessor customer-name
    :documentation "Customer's name")
   (balance
    :initarg :balance
    :initform 0
    :reader balance
    :documentation "Current account balance")
   (account-number
    :initform (incf *account-numbers*)
    :reader account-number
    :documentation "Account number, unique within a bank.")
   (account-type
    :reader account-type
    :documentation "Type of account, one of :gold, :silver, or :bronze.")))

(defmethod assess-low-balance-penalty ((account bank-account))
  (with-slots (balance) account
    (when (< balance *minimum-balance*)
      (decf balance (* balance .01)))))

(defmethod merge-accounts ((account1 bank-account) (account2 bank-account))
  (with-accessors ((balance1 balance)) account1
    (with-accessors ((balance2 balance)) account2
      (incf balance2 balance2)
      (setf balance2 0))))

;;; slots and inheritance
(defclass foo ()
  ((a :initarg :a :initform "A" :accessor a)
   (b :initarg :b :initform "B" :accessor b)))

(defclass bar (foo)
  ((a :initform (error "Must supply a value for a"))
   (b :initarg :the-b :accessor the-b :allocation :class)))
