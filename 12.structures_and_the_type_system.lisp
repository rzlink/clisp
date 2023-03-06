(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))

(defvar s1 (make-starship))

(setf s1 '#S(starship speed (warp 3) condition red shields up))

(starship-p s1)
(typep s1 'starship)
(type-of s1)
(starship-speed s1)
(starship-shields s1)
(setf (starship-name s1) "Enterprise")

(defvar s2 (make-starship))
(incf (starship-speed s2))

(defun alert (x)
  (setf (starship-shields x) 'up)
  (if (equal (starship-condition x) 'green)
      (setf (starship-condition x) 'yellow))
  'shields-raised)

(alert s1)

;;; KEYWORD ARGUMENTS TO CONSTRUCTOR FUNCTIONS
(setf s3 (make-starship :name "Reliant"
                        :shields 'damaged))

;;; Lisp Toolkit: Describe and Inspect
(describe 7)
(describe 'fred)
(describe t)
(describe 'cons)
(describe s1)

(inspect 'car)

;;; Keyboard Exercise
;;; 12.4
(defstruct node (name) (question) (yes-case) (no-case))

(defvar *node-list* nil)

(defun init ()
  (setf *node-list* nil))

(defun add-node (name question yes-case no-case)
  (let ((node (make-node
               :name name
               :question question
               :yes-case yes-case
               :no-case no-case)))
    (push node *node-list*)
    (node-name node)))

(defun find-node (name)
  (find-if #'(lambda (node)
               (equal (node-name node) name))
           *node-list*))

(defun process-node (name)
  (let ((node (find-node name)))
   (if node
       (yes-or-no-p
        (node-question node)
        (node-yes-case node)
        (node-no-case node))
       (format t "~&Node hasn't been defined yet."))) )

(defun run ()
  (let ((current-node (find-node 'start)))))
