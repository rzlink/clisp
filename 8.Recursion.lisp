(setf *family*
      '((colin nil nil)
        (deirdre nil nil)
        (arthur nil nil)
        (kate nil nil)
        (frank nil nil)
        (linda nil nil)
        (suzanne colin deirdre)
        (bruce arthur kate)
        (charles arthur kate)
        (david arthur kate)
        (ellen arthur kate)
        (george frank linda)
        (hillary frank linda)
        (andre nil nil)
        (tamara bruce suzanne)
        (vincent bruce suzanne)
        (wanda nil nil)
        (ivan george ellen)
        (julie george ellen)
        (marie george ellen)
        (nigel andre hillary)
        (frederick nil tamara)
        (zelda vincent wanda)
        (joshua ivan wanda)
        (quentin nil nil)
        (robert quentin julie)
        (olivia nigel marie)
        (peter nigel marie)
        (erica nil nil)
        (yvette robert zelda)
        (diane peter erica)))

;;; 8.60.a
(defun father (x)
  (second (assoc x *family*)))

(defun mother (x)
  (third (assoc x *family*)))

(defun parents (x)
  (union (and (father x) (list (father x)))
         (and (mother x) (list (mother x)))))

(defun children (x)
  (remove nil (mapcar #'(lambda (relation)
                          (if (or (equal (second relation) x)
                                  (equal (third relation) x))
                              (car relation)))
                      *family*)))

;;; 8.60.b
(defun siblings (x)
  (set-difference (union (children (father x))
                         (children (mother x)))
                  (list x)))

;;; 8.60.c
(defun mapunion (fn lst)
  (reduce #'union (mapcar fn lst)))

;;; 8.60.d
(defun grandparents (x)
  (mapunion #'parents (parents x)))

;;; 8.60.e
(defun cousins (x)
  (mapunion #'children (mapunion #'siblings  (parents x))))

;;; 8.60.f
(defun descended-from (x y)
  (cond ((or (null x)) nil)
        ((member y (parents x)) t)
        (t (or (descended-from (father x) y)
               (descended-from (mother x) y)))))

;;; 8.60.g
(defun ancestrors (x)
  (cond ((null x) nil)
        (t (union
            (parents x)
            (union (ancestrors (father x))
                   (ancestrors (mother x)))))))

;;; 8.60.h
(defun generation-gap (x y)
  (g-gap-helper x y 0))

(defun g-gap-helper (x y n)
  (cond ((null x) nil)
        ((equal x y) n)
        (t (or (g-gap-helper
                (father x) y (1+ n))
               (g-gap-helper
                (mother x) y (1+ n))))))

;;; 8.61
(defun tr-count-up (n)
  ())
