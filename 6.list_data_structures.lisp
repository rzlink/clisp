(defun add-to-end (x e)
  "Adds element E to the end of list X."
  (append x (list e)))

;;; 6.6
(defun last-element (x)
  (first (last x)))

(defun last-element (x)
  (first (reverse x)))

(defun last-element (x)
  (and x
       (nth (1- (length x)) x)))

;;; 6.7
(defun next-to-last (x)
  (second (reverse x)))

(defun next-to-last (x)
  (and (rest x)
       (nth (- (length x) 2) x)))

;;; 6.8
(defun my-butlast (x)
  (reverse (rest (reverse x))))

;;; 6.9
(defun mystery (x)
  (first (last (reverse x))))

;;; 6.10
(defun palindromep (x)
  (equal (reverse x) x))

;;; 6.11
(defun make-palindrome (x)
  (append x (reverse x)))

(defun beforep (x y l)
  "Returns true if X appears before Y in L"
  (member y (member x l)))

;;; 6.15
(defun contains-article-p (sent)
  (intersection sent '(the a an)))

(defun contains-article-p (sent)
  (or (member 'the sent)
      (member 'a sent)
      (member 'an sent)))

;;; 6.18
(defun add-vowels (x)
  (union x '(a e i o u)))

;;; 6.21
(defun my-subsetp (x y)
  (null (set-difference x y)))

;;; 6.24
(defun set-equal (x y)
  (and (subsetp x y) (subsetp y x)))

;;; 6.25
(defun proper-subsetp (x y)
  (and (subsetp x y) (not (subsetp y x))))

;;; Programming with sets
(defun titledp (name)
  (member (first name) '(mr ms miss mrs)))

(setf male-first-names
      '(john kim richard fred george))

(setf female-first-names
      '(jane mary wanda barbara kim))

(defun malep (name)
  (and (member name male-first-names)
       (not (member name female-first-names))))

(defun femalep (name)
  (and (member name female-first-names)
       (not (member name male-first-names))))

(defun give-title (name)
  "Returns a name with an appropriate title on
   the front."
  (cond ((titledp name) name)
        ((malep (first name)) (cons 'mr name))
        ((femalep (first name)) (cons 'ms name))
        (t (append '(mr or ms) name))))

;;; 6.26
(defun right-side (x)
  (rest (member '-vs- x)))

(defun left-side (x)
  (reverse (right-side (reverse x))))

(defun count-common (x)
  (length (intersection (left-side x) (right-side x))))

(defun compare (x)
  (list (count-common x) 'common 'features))

;;; 6.27
(defvar *produce* '((apple . fruit)
                    (celery . veggie)
                    (banana . fruit)
                    (lettuce . veggie)))

(defvar *things*
  '((object1 large green shiny cube)
    (object2 small red dull metal cube)
    (object3 red small dull plastic cube)
    (object4 small dull blue metal cube)
    (object5 small shiny red four-sided pyramid)
    (object6 large shiny green sphere)))

(defun description (x)
  (rest (assoc x *things*)))

(defun differences (x y)
  (set-exclusive-or (description x)
                    (description y)))

(defvar *quality-table*
  '((large . size)
    (small . size)
    (red . color)
    (green . color)
    (blue . color)
    (shiny . luster)
    (dull . luster)
    (metal . material)
    (plastic . material)
    (cube . shape)
    (sphere . shape)
    (pyramid . shape)
    (four-sided . shape)))

(defun quality (x)
  (cdr (assoc x *quality-table*)))

(defun quality-difference (x y)
  (quality (first (differences x y))))

(defun contrast (x y)
  (remove-duplicates
   (sublis *quality-table* (differences x y))))

;;;
(defvar *books*
  '((war-and-peace leo-tolstoy)
    (jin-ping-mei jinyong)
    (xi-you-ji wu-cheng-en)
    (shui-hu-zhuan shi-nai-en)
    (hong-long-meng cao-xue-qin)))

(defun who-write (book)
  (second (assoc book *books*)))

;;; 6.35
(defvar *nerd-states*
  '((sleeping . eating)
    (eating . waiting-for-a-compute)
    (waiting-for-a-compute . programming)
    (programming . debugging)
    (debugging . sleeping)))

(defun nerdus (state)
  (cdr (assoc state *nerd-states*)))

(defun sleepless-nerd (state)
  (let ((y (nerdus state)))
    (if (equal y 'sleeping)
        (nerdus y)
        y)))

(defun nerd-on-caffeine (state)
  (nerdus (nerdus state)))

;;; 6.36
(defun swap-first-last (x)
  (let* ((a (reverse (rest x)))
         (b (reverse (rest a))))
    (cons (first a)
          (append b (list (first x))))))

;;; 6.37
(defun rotate-left (x)
  (append (rest x) (list (first x))))

(defun rotate-right (x)
  (let ((y (reverse x)))
    (cons (first y) (reverse (rest y)))))

;;; 6.38
(defun )

;;; 6.41 keyboard exercise
(defvar *rooms*
  '((living-room
     (north front-stairs)
     (south dining-room)
     (east kitchen))
    (upstairs-bedroom
     (west library)
     (south front-stairs))
    (dining-room
     (north living-room)
     (east pantry)
     (west downstairs-bedroom))
    (kitchen
     (west living-room)
     (south pantry))
    (pantry
     (north kitchen)
     (west dining-room))
    (downstairs-bedroom
     (north back-stairs)
     (east dining-room))
    (back-stairs
     (south downstairs-bedroom)
     (north library))
    (front-stairs
     (north upstairs-bedroom)
     (south living-room))
    (library
     (east upstairs-bedroom)
     (south back-stairs))))

(defun choices (room)
  (rest (assoc room *rooms*)))

(defun look (direction room)
  (rest (assoc direction (choices room))))

(defvar *loc* nil)

(defun set-robbie-location (place)
  "Moves Robbie to PLACE by setting
   the variable LOC."
  (setf *loc* place))

(defun how-many-choices ()
  (length (choices *loc*)))

(defun upstairsp (loc)
  (or (equal loc 'library)
      (equal loc 'upstairs-bedroom)))

(defun onstairsp (loc)
  (or (equal loc 'front-stairs)
      (equal loc 'back-stairs)))

(defun where ()
  (cond ((upstairsp *loc*)
         (append '(robbie is upstairs in the) (list *loc*)))
        ((onstairsp *loc*)
         (append '(robbie is downstairs in the) (list *loc*)))
        (t
         (append '(robbie is on the) (list *loc*)))))

(defun move (direction)
  (let ((loc (look direction *loc*)))
    (cond ((null loc)
           '(OUCH! robbie hit a wall))
          (t
           (set-robbie-location loc)
           (where)))))

;;; 6.42
(defun royal-we (sent)
  (subst 'we 'i sent))
