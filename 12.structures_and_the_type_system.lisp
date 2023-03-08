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
    name))

(defun find-node (name)
  (find-if #'(lambda (node)
               (equal (node-name node) name))
           *node-list*))

(defun process-node (name)
  (let ((node (find-node name)))
   (if node
       (if (yes-or-no-p "~&~A " (node-question node))
           (node-yes-case node)
           (node-no-case node))
       (format t "~&Node ~S hasn't been defined yet." name))) )

(defun run ()
  (do ((current-node 'start (process-node current-node)))
      ((null current-node) nil)
    (cond ((stringp current-node)
           (format t "~&~A" current-node)
           (return nil)))))

(defun interactive-add ()
  (let ((name (prompt-for "Node name? "))
        (quest (prompt-for "Question? "))
        (yes-action (prompt-for "If yes? "))
        (no-action (prompt-for "If no? ")))
    (add-node name quest yes-action no-action)))

(defun prompt-for (prompt-string)
  (format t "~A" prompt-string)
  (read))

(add-node 'start
          "Does the engine turn over?"
          'engine-turns-over
          'engine-wont-turn-over)

(add-node 'engine-turns-over
          "will the engine run for any period of time?"
          'engine-will-run-briefly
          'engine-wont-run)

(add-node 'engine-wont-run
          "Is there gas in the tank?"
          'gas-in-tank
          "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
          "Do you hear any sound when you turn the key?"
          'sound-when-turn-key
          'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
          "Is the battery voltage low?"
          "Replace the battery"
          'battery-voltage-ok)

(add-node 'battery-voltage-ok
          "Are the battery cables dirty or loose?"
          "Clean the cables and tighten the connections."
          'battery-cabls-good)

;;; 12.8 PRINT FUNCTIONS FOR STRUCTURES
(defun print-starship (x stream)
  (format stream "#<STARSHIP ~A>"
          (starship-name x)))

(print-starship s1 t 0)

(defstruct (starship
            (:print-function print-starship))
  (captain nil)
  (name nil)
  (shields 'down)
  (condition 'green)
  (speed 0))

(defparameter s4 (make-starship :name "Reliant"))

;;; 12.9 EQUALITY OF STRUCTURES
(defvar s5 (make-starship))

(defvar s6 (make-starship))

(equal s5 s6)
(equal s6 s6)

(equalp s5 s6)

;;; INHERITANCE FROM OTHER STRUCTURS
(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))

(defstruct (starship (:include ship))
  (weapons nil)
  (shields nil))

(defstruct (supply-ship (:include ship))
  (cargo nil))


(defvar z1 (make-starship
            :captain "James T. Kirk"))

(defvar z2 (make-supply-ship
            :captain "Harry Mudd"))

(ship-p z1)
(starship-p z1)
(supply-ship-p z1)

(ship-captain z1)
(sharship-captain z1)
