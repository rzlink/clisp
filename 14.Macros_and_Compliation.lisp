;;; 14.3
(defmacro set-nil (x)
  `(setq ,x nil))

(setf name 'fred)

'(i gave ,anme about ,(* 25 8) dollars)


(defmacro simple-incf (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))

(defvar fred-loan 10)

(simple-incf fred-loan (* 25 8))

;;; 14.4
(defmacro simple-rotatef (a b)
  (let ((temp (gensym)))
    `(let ((,temp nil))
       (setq ,temp ,a)
       (setq ,a ,b)
       (setq ,b ,temp))))

(setf a 10)
(setf b 20)
(simple-rotatef a b)

(defmacro two-from-one (func object)
  `(,func ',object ',object))

(two-from-one cons aardvark)

;;; 14.5
(defmacro set-mutual (var1 var2)
  `(progn
     (setf ,var1 ',var2)
     (setf ,var2 ',var1)))

(defun f (x y)
  (showvar x)
  (showvar y)
  (* x y))

(defmacro showvar (var)
  `(format t "~&The value of ~S is ~S" ',var ,var))

(f 3 7)

;;; SPLICING WITH BACKQUOTE
(setf name 'fred)
(setf address '(16 maple drive))

`(,name lives at ,address now)
`(,name lives at ,@address now)

(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (x)
                       (list 'setf x 0))
                    variables)
          '(zeroed ,@variables)))

(macroexpand-1 '(set-zero a b c))

(macroexpand-1 '(cond ((null lst) (print "hello"))))

;;; 14.6
(defmacro variable-chain (&rest variables)
  `(progn
     ,@(do*
       ((vars variables (rest vars))
        (x (first vars) (first vars))
        (y (second vars) (second vars))
        (result nil))
       ((null y) (reverse result))
        (push (list 'setf x `',y) result))))

(defmacro variable-chain (&rest variables)
  `(progn
    ,@(do ((var variables (rest var))
           (result nil))
          ((null (rest var)) (reverse result))
        (push `(setf ,(first var)
                     ',(second var))
              result))))

(macroexpand-1 '(variable-chain a b c d))

;;; 14.8 THE COMPILER
(defun tedious-sqrt (n)
  (dotimes (i n)
    (if (> (* i i) n) (return i))))

(time (tedious-sqrt 50000000))

(compile 'tedious-sqrt)


;;; 14.11 CASE STUDY: FINITE STATE MACHINES

(defstruct (node (:print-function print-node))
  (name nil)
  (inputs nil)
  (outputs nil))

(defun print-node (node stream depth)
  (format stream "#<Node ~A>"
          (node-name node)))

(defstruct (arc (:print-function print-arc))
  (from nil)
  (to nil)
  (label nil)
  (action nil))

(defun print-arc (arc stream depth)
  (format stream "#<ARC ~A / ~A / ~A>"
          (node-name (arc-from arc))
          (arc-label arc)
          (node-name (arc-to arc))))

(defvar *nodes*)
(defvar *arcs*)
(defvar *current-node*)

(defun initialize ()
  (setf *nodes* nil)
  (setf *arcs* nil)
  (setf *current-node* nil))

(defmacro defnode (name)
  `(add-node ',name))

(defun add-node (name)
  (let ((new-node (make-node :name name)))
    (setf *nodes* (nconc *nodes* (list new-node)))
    new-node))

(defun find-node (name)
  (or (find name *nodes* :key #'node-name)
      (error "no node named ~A exists." name)))

(defmacro defarc (from label to &optional action)
  `(add-arc ',from ',label ',to ',action))

(defun add-arc (from-name label to-name action)
  (let* ((from (find-node from-name))
         (to (find-node to-name))
         (new-arc (make-arc :from from
                            :label label
                            :to to
                            :action action)))
    (setf *arcs* (nconc *arcs* (list new-arc)))
    (setf (node-outputs from)
          (nconc (node-inputs from)
                 (list new-arc)))
    (setf (node-inputs to)
          (nconc (node-inputs to)
                 (list new-arc)))
    new-arc))

(defnode start)
(defnode have-5)
(defnode have-10)
(defnode have-15)
(defnode have-20)
(defnode end)

(defarc start nickel have-5 "Clunk!")
(defarc start dime have-10 "Clink!")
(defarc start coin-return start "Nothing to return.")
(defarc have-5 nickel have-10 "Clunk!")
(defarc have-5 dime have-15 "Clink!")
(defarc have-5 coin-return start "Returned five cents.")
(defarc have-10 nickel have-15 "Clunk!")
(defarc have-10 dime have-20 "Clink!")
(defarc have-10 coint-return start "Returned ten cents.")
(defarc have-15 nickel have-20 "Clunk!")
(defarc have-15 dime have-20 "Nickel change.")
(defarc have-15 gum-button end "Deliver gum.")
(defarc have-15 coin-return start "Returned fifteen cents.")
(defarc have-20 nickel have-20 "Nickel returned.")
(defarc have-20 dime have-20 "Dime returned.")
(defarc have-20 gum-button end "Deliver gum, nickel change.")
(defarc have-20 mint-button end "Deliver mints.")
(defarc have-20 coin-return start "Returned twenty cents.")

(defun fsm (&optional (starting-point 'start))
  (setf *current-node* (find-node starting-point))
  (do (_)
      ((null (node-outputs *current-node*)))
    (one-transition)))

(defun one-transition ()
  (format t "~&State ~A. Input: "
          (node-name *current-node*))
  (let* ((ans (read))
         (arc (find ans
                    (node-outputs *current-node*)
                    :key #'arc-label)))
    (unless arc
      (format t "~&No arc from ~A has label ~A.~%"
              (node-name *current-node*) ans)
      (return-from one-transition nil))
    (let ((new (arc-to arc)))
      (format t "~&~A" (arc-action arc))
      (setf *current-node* new))))
