﻿(setq dimension 0)
(setq states nil)
(setq states-vertical nil)

(defun start()
  (format t "~% Izaberite mod igre [mod]..")
  (format t "~% [1] Covek-racunar :: x o")
  (format t "~% [2] Racunar-covek :: x o")
  (format t "~% [3] Covek-covek :: x o")
  (format t "~% [exit] Izlaz ~%")
  
  (let 
   ((mode (read)))
    (cond
     ((equalp mode 1)
      (progn 
        (form-matrix)
        (show-output (states-to-matrix 1 dimension states))
        (make-move t))
     )
     ((equalp mode 2) nil)
     ((equalp mode 3) 
      (progn 
         (form-matrix)
         (show-output (states-to-matrix 1 dimension states))
         (make-move t)
      ))
     ((string-equal mode "exit") #+sbcl (sb-ext:quit))
     (t (format t "~% Nepravilan mod ~%~%") (start))
)))

(defun form-matrix ()
  (format t "~% Unesite dimenziju table za Hashami igru, dimenzija treba da bude u opsegu 9-11~%")
  (setq dimension (read))
  (cond
   ((< dimension 9) (format t "~% Dimenzija table je premala") (form-matrix))
   ((> dimension 11) (format t "~% Dimenzija table je prevelika") (form-matrix))
   (t (progn (setq states-vertical (initial-states-vertical dimension)) (setq states (initial-states dimension))))
  )
)

(defun make-move (xo)  ; xo true : x | false: o za zaizmenicne poteze
  (format t "~%~%~A: unesite potez oblika ((x y) (n m)): " (if xo #\x #\o))
  (let* ((input (read)) 
         (current (form-move (car input))) 
         (move (form-move (cadr input))) 
         (player (if xo #\x #\o))
         (horizontal (states-to-matrix 1 dimension states))
         (vertical (states-to-matrix 1 dimension states-vertical))
         )
    (cond
     ((string-equal (caar input) "exit") #+sbcl (sb-ext:quit))
     ((or (null current) (null move) (> (car current) dimension) (> (cadr current) dimension) (> (car move) dimension) (> (cadr move) dimension)) (format t "~%~%Nepravilan format ili granice polja..~%") (make-move xo)) ; nepravilno formatiran unos poteza rezultuje ponovnim unosom istog poteza
     (t (if (or (validate-state current move (generate-states horizontal 1 xo))
                (validate-state (list (cadr current) (car current)) (list (cadr move) (car move)) (generate-states vertical 1 xo))) 
          (progn 
          (change-state (car current) (cadr current) (car move) (cadr move) xo)
          (let*
          ((horizontal-coded (states-to-matrix 1 dimension states))
           (vertical-coded (states-to-matrix 1 dimension states-vertical)))
           ; matrice kodiranja koje mogu da se pre povlacenja poteza prolsedjuju (validate-move ..))    
            (cond
             ((or (check-winner-state-horizontal (nth (1- (car move)) horizontal-coded) (car move) xo 0) (check-winner-state-vertical (nth (1- (cadr move)) vertical-coded) (cadr move) xo 0)) (progn (print-matrix horizontal-coded) (format t "~%~%Pobednik je ~A ~%~%" (if xo #\x #\o)) #+sbcl (sb-ext:quit)))
             (t (show-output horizontal-coded) (make-move (not xo)))))
           )  
          (progn (format t "~%~%nedozvoljen potez, pokusajte ponovo..~%") (make-move xo)))
))))

(defun form-move (move)
  (if (and (member (car move) '(A B C D E F G H I J K)) (member (cadr move) '(1 2 3 4 5 6 7 8 9 10 11)))
      (cond 
       ((equal (car move) 'a) (list '1 (cadr move)))
       ((equal (car move) 'b) (list '2 (cadr move)))
       ((equal (car move) 'c) (list '3 (cadr move)))
       ((equal (car move) 'd) (list '4 (cadr move)))
       ((equal (car move) 'e) (list '5 (cadr move)))
       ((equal (car move) 'f) (list '6 (cadr move)))
       ((equal (car move) 'g) (list '7 (cadr move)))
       ((equal (car move) 'h) (list '8 (cadr move)))
       ((equal (car move) 'i) (list '9 (cadr move)))
       ((equal (car move) 'j) (list '10 (cadr move)))
       ((equal (car move) 'k) (list '11 (cadr move)))
       (t '())
      )
    '()
))

(defun check-winner-state-horizontal (coded-row rownum xo counter) ; rownum za broj vrste | coded-row (nth rownum-1 horizontal-matrix)
  (cond
   ((null coded-row) nil)
   ((and xo (<= rownum 2)) nil)
   ((and (not xo) (> rownum (- dimension 2))) nil)
   ((equalp counter 5) t)
   ((and (listp (car coded-row)) (equalp (cadar coded-row) (if xo 'x 'o))) (check-winner-state-horizontal (cdr coded-row) rownum xo (1+ counter)))
   (t (check-winner-state-horizontal (cdr coded-row) rownum xo 0))
  )
)

(defun check-winner-state-vertical (coded-column rownum xo counter) ; rownum za broj vrste i uvek se prosledjuje 1 i inkrementira se kroz funkciju
  (cond
   ((null coded-column) nil)
   ((and (not xo) (> rownum (- dimension 2))) nil)
   ((equalp counter 5) t)
   ((and (listp (car coded-column)) (equalp (cadar coded-column) (if xo 'x 'o)) (and xo (> rownum 2))) (check-winner-state-vertical (cdr coded-column) (1+ rownum) xo (1+ counter)))
   ((listp (car coded-column)) (check-winner-state-vertical (cdr coded-column) (1+ rownum) xo 0))
   (t (check-winner-state-vertical (cdr coded-column) (+ rownum (car coded-column)) xo 0))
  )
)

(defun make-all-states (all-states xo invert)
  (cond
   ((null all-states) nil)
   ((not (null (cadar all-states))) (append (make-states (caaar all-states) (cadaar all-states) (cadar all-states) xo invert) (make-all-states (cdr all-states) xo invert)))
   (t (make-all-states (cdr all-states) xo invert))
  )
)

(defun make-states (x y possible xo invert)
  (cond
   ((null possible) nil)
   (t (cons (make-state x y (caar possible) (cadar possible) xo invert) (make-states x y (cdr possible) xo invert)))
  )
)

(defun reverse-all (to-reverse)
  (cond
   ((null to-reverse) nil)
   (cond (reverse (car to-reverse)) (reverse-all (cdr to-reverse)))
  )
)

(defun make-state (x y x-new y-new xo invert)
  (cond
   ((and (not invert) xo) (progn 
         (list
         (list (insert-state x-new y-new (remove-state x y (car states))) (cadr states))
         (list (insert-state y-new x-new (remove-state y x (car states-vertical))) (cadr states-vertical))
         )))
   ((and (not invert) (not xo)) (progn
         (list 
         (list (car states) (insert-state x-new y-new (remove-state x y (cadr states))))
         (list (car states-vertical) (insert-state y-new x-new (remove-state y x (cadr states-vertical))))  
         )))
   ((and invert xo) (progn 
         (list 
         (list (insert-state y-new x-new (remove-state y x (car states))) (cadr states))
         (list (insert-state x-new y-new (remove-state x y (car states-vertical))) (cadr states-vertical))
         )))
   ((and invert (not xo)) (progn
         (list 
         (list (car states) (insert-state y-new x-new (remove-state y x (cadr states))))  
         (list (car states-vertical) (insert-state x-new y-new (remove-state x y (cadr states-vertical))))  
         )))
  )
)

(defun validate-state (source destination all-states)
  (cond
   ((null all-states) nil)
   ((and (equalp source (caar all-states)) (member destination (cadar all-states) :test 'equal)) t)
   (t (validate-state source destination (cdr all-states)))
  )
)


(defun generate-states (matrix lvl xo)
  (cond
   ((null matrix) nil)
   (t (append (generate-moves-for-row lvl nil nil (if xo 'x 'o) (car matrix) nil) (generate-states (cdr matrix) (1+ lvl) xo)))
  )
)

;funkcija za generisanje poteza u jednom redu, ulazni parametri - lvl (koji red evaluiramo), seclst (predzadnji element), lst (prethodni element), xo (kog igra?a evaluiramo), row - (kodirani red), res (rezultat), izlaz - lista sa u formatu (((trenutna figura - koordinate)((moguca nova pozicija 1) (moguca nova pozicija 2)...))(...))
(defun generate-moves-for-row (lvl seclst lst xo row res)

  (let* ((value (encode-element (car row) xo)))
    (cond
      ((and (null seclst) (null lst)) (generate-moves-for-row lvl lst value xo (cdr row) res))
      ((null row) res)
;      ((zerop value) (generate-moves-for-row lvl lst 0 xo (cdr row) res ))
      ((atom value) (cond
                      ((zerop value) (generate-moves-for-row lvl lst 0 xo (cdr row) res ))
                      ((listp lst) (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl lst value NIL))))
                      ((zerop lst)(cond
                                    ((and (not(null seclst)) (listp seclst))(generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl seclst 0 T))))
                                    (t (generate-moves-for-row lvl lst value xo (cdr row) res))))
                      ))
      ((and(atom lst) (not (zerop lst))) (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl value lst T))))
       (t (generate-moves-for-row lvl lst value xo (cdr row) res))
       )
    )
)

;pomo?na funkcija za generate-moves for row, ulazni parametri - el (element koji ispitujemo), xo (kog igra?a evaluiramo), izlaz - ako je element koordinata igra?a koji nas interesuje onda vra?amo tu koordinatu, ako je od protivnika - vra?amo nulu, ako je broj slobodnih mesta - vra?amo ga takvog kakav je
(defun encode-element (el xo)

  (cond
    ((listp el)(cond
                 ((equalp (cadr el) xo) el)
                 (t 0)))
    (t el)
    )
)

(defun append-moves-for-row (lvl el size prev)
    (list(cons (list lvl (car el)) (list (cond
                                        ((zerop size) (list lvl (+ (car el) 2)))
                                        (t (loop for x from 1 to size collect (list lvl (cond
                                                                                          ((null prev)(+ (car el) x))
                                                                                          (t (- (car el) x))))))))))
)

(defun insert-state (x y rearanged-states)
  (cond
   ((null rearanged-states) (list (list x y)))
   ((or (and (equalp (caar rearanged-states) x) (< y (cadar rearanged-states))) (< x (caar rearanged-states))) (cons (list x y) rearanged-states))
   (t (cons (car rearanged-states) (insert-state x y (cdr rearanged-states))))
  )
)

(defun remove-state (x y changed-states)
  (cond
   ((null changed-states) nil)
   ((and (equalp (caar changed-states) x) (equalp y (cadar changed-states))) (cdr changed-states))
   (t (cons (car changed-states) (remove-state x y (cdr changed-states))))
  )
)

(defun change-state (x y x-new y-new xo)
  (cond
   (xo (progn 
         (setq states-vertical (list (insert-state y-new x-new (remove-state y x (car states-vertical))) (cadr states-vertical)))
         (setq states (list (insert-state x-new y-new (remove-state x y (car states))) (cadr states)))
         ))
   ((not xo) (progn
         (setq states-vertical (list (car states-vertical) (insert-state y-new x-new (remove-state y x (cadr states-vertical)))))
         (setq states (list (car states) (insert-state x-new y-new (remove-state x y (cadr states)))))      
         ))
  )
)

(defun initial-row (row column) 
  (cond
    ((zerop row) nil)
    (t (append (initial-row (- row 1) column) (list (list column row))))
    )
)

;;funkcija koja defnise inicijalno stanje table u formi
;;((lista figura prvog igraca) (lista figura drugog igraca))
(defun initial-states (dim)
  (list (append (initial-row dim 1) (initial-row dim 2)) (append (initial-row dim (- dim 1)) (initial-row dim dim)) )
)

(defun initial-states-vertical (dim)
  (list (initial-column dim) (initial-column-extend dim))
)

(defun initial-column (dim)
  (cond
   ((zerop dim) nil)
   (t (append (initial-column (- dim 1)) (list (list dim 1) (list dim 2))))
  )
)

(defun initial-column-extend (dim)
  (cond
   ((zerop dim) nil)
   (t (append (initial-column-extend (- dim 1)) (List (list dim (- dimension 1)) (list dim dimension))))
  )
)

(defun show-initial-matrix (dim)
  (print-matrix(states-to-matrix 1 dim  (initial-states dim)))
)

(defun print-matrix (mat indices)
  (cond
    ((null mat) NIL)
    (t (format t "~a " (car indices)) (print-row (car mat)) (print-matrix (cdr mat) (cdr indices)))
  )
)

(defun show-output (matrix)
  (format t "~%  ") 
  (show-indices dimension '(1 2 3 4 5 6 7 8 9 10 11))
  (print-matrix matrix '(A B C D E F G H I J K L))
)


(defun show-indices (ith lst)
  (cond
   ((equalp ith 1) (format t " ~a ~%" (car lst)))
   ((not (zerop ith)) (format t " ~a " (car lst)) (show-indices (1- ith) (cdr lst)))
   (t nil)
  )
)

;; funkcija za stampanje reda matrice, prosledjenog u formi liste atoma, gde
;; pozitivna vrednost oznacava prazna polja a sama velicina vrednosti
;; broj uzastopnih blanko polja, negativna vrednost oznacava "o", a nula "x"
(defun print-row (row)
  (cond
    ((null row) (fresh-line))
    ((atom (car row)) (print-blank (car row)) (print-row (cdr row)) )
   ;; ((zerop (car row)) (format t "x ") (print-row (cdr row)))
    (t (format t " ~a " (cadar row)) (print-row (cdr row)))
    )
)

;; Pomocna funkcija za stampanje reda,koristi se za uzastopno stampanje
;; blanko znaka.
(defun print-blank (blanks)
  (cond
    ((zerop blanks) nil)
    (t (format t " - ") (print-blank (- blanks 1)) )
  )
)

(defun states-to-matrix (lvl dim states)
  (cond
    ((> lvl dim ) nil)
    (t (let* ((value (encode-row lvl dim (car states) (cadr states) nil 0))) (append (list (car value)) (states-to-matrix (+ lvl 1) dim (cadr value)))))
    )
)

(defun encode-row (lvl dim fst sec res sum)
  (cond
    ((null (next-value lvl fst sec)) (cond
                                       ((equalp dim sum) (list res (list fst sec)))
                                       (t (list (append res (list(- dim sum))) (list fst sec)))))
    (t (let* ((value (next-value lvl fst sec)))
         (cond
           ((equalp dim (caar value))  (list (append res (cond ((equalp (- (caar value) 1) sum )(list (car value)))
                                                               (t (list (- (caar value) sum 1) (car value))))) (list (cadr value) (caddr value))))
           ((null res)(encode-row lvl dim (cadr value) (caddr value) (cond ((equalp (caar value) 1) (list(car value)))
                                                                           (t (list (- (caar value) 1) (car value)))) (caar value) ))
           ((equalp (- (caar value) 1) sum) (encode-row lvl dim (cadr value) (caddr value) (append res (list(car value))) (caar value)) )
           (t (encode-row lvl dim (cadr value) (caddr value) (append res (list (- (caar value) sum 1) (car value))) (caar value)))
           ))
       )
    )
)

(defun next-value (lvl fst sec)
  (cond
    ((equalp (caar fst) lvl) (cond
                                   ((equalp (caar sec) lvl) (cond
                                                              ((< (cadar fst) (cadar sec)) (list (list (cadar fst) 'x) (cdr fst) sec))
                                                             (t (list (list (cadar sec) 'o) fst (cdr sec)))))
                                   (t (list (list (cadar fst) 'x) (cdr fst) sec))))
    ((equalp (caar sec) lvl) (list (list (cadar sec) 'o) fst (cdr sec)))
    (t nil)
    )
)