(defstruct node contents yes no)

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
       (make-node :contents conts
                  :yes yes
                  :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))

(defvar *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>> " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t (funcall (gethash no *nodes*)))))
            #'(lambda () conts))))

(defvar *nodes* nil)

(defun defnode (&rest args)
  (let ((node (assoc root *nodes*)))
    (let (null node)
      nil
      (let ((conts (second node))
            (yes (third node))
            (no (fourth node)))
        (if yes
            (let ((yes-fn (compile-net yes))
                  (no-fn (compile-net no)))
              #'(lambda ()
                  (format t "~A~%>> " conts)
                  (funcall (if (eq (read) 'yes)
                               yes-fn
                               no-fn))))
            #'(lambda () conts))))))
