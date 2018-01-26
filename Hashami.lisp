(defvar dimension 0)
(defvar states nil)
(defvar states-vertical nil)

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
        (make-move-ai t nil))
     )
     ((equalp mode 2)
       (progn
        (form-matrix)
        (make-move-ai t t))
     )
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
         (horizontal (states-to-matrix 1 dimension states))
         (vertical (states-to-matrix 1 dimension states-vertical))
         )
    (cond
     ((string-equal (caar input) "exit") #+sbcl (sb-ext:quit))
     ((or (null current) (null move) (> (car current) dimension) (> (cadr current) dimension) (> (car move) dimension) (> (cadr move) dimension)) (format t "~%~%Nepravilan format ili granice polja..~%") (make-move xo)) ; nepravilno formatiran unos poteza rezultuje ponovnim unosom istog poteza
     (t (if (validate-users-move current move horizontal vertical xo)
          (progn
            (change-state (car current) (cadr current) (car move) (cadr move) xo)
            (let*
              ((horizontal-coded (states-to-matrix 1 dimension states))
               (vertical-coded (states-to-matrix 1 dimension states-vertical)))
               (if (evaluate-winner horizontal-coded vertical-coded move xo)
                  (make-move (not xo)))
            )
        )

        (progn (format t "~%~%nedozvoljen potez, pokusajte ponovo..~%") (make-move xo))
)))))

(defun make-move-ai (xo artifficial)
  (if artifficial
  (let*
    ((new-states (alpha-beta  (list states states-vertical) -5000 5000 4 xo)))
    (progn
      (setq states (car new-states))
      (setq states-vertical (cadr new-states))
      (if (evaluate-winner-ai (states-to-matrix 1 dimension states) (states-to-matrix 1 dimension states-vertical) xo)
          (progn
            (show-output (states-to-matrix 1 dimension states))
            (format t "~%~%Pobednik je ~A ~%~%" (if xo #\x #\o)) #+sbcl (sb-ext:quit)
          )
          (make-move-ai (not xo) (not artifficial))
      )
   )
  )

    (progn
    (show-output (states-to-matrix 1 dimension states))
    (format t "~%~%~A: unesite potez oblika ((x y) (n m)): " (if xo #\x #\o))
    (let* ((input (read))
           (current (form-move (car input)))
           (move (form-move (cadr input)))
           (horizontal (states-to-matrix 1 dimension states))
           (vertical (states-to-matrix 1 dimension states-vertical))
           )
        (cond
         ((string-equal (caar input) "exit") #+sbcl (sb-ext:quit))
         ((or (null current) (null move) (> (car current) dimension) (> (cadr current) dimension) (> (car move) dimension) (> (cadr move) dimension)) (format t "~%~%Nepravilan format ili granice polja..~%") (make-move-ai xo artifficial)) ; nepravilno formatiran unos poteza rezultuje ponovnim unosom istog poteza
         (t (if (validate-users-move current move horizontal vertical xo)
              (progn
                (change-state (car current) (cadr current) (car move) (cadr move) xo)
                (let*
                  ((horizontal-coded (states-to-matrix 1 dimension states))
                   (vertical-coded (states-to-matrix 1 dimension states-vertical)))
                   (if (evaluate-winner horizontal-coded vertical-coded move xo)
                      (make-move-ai (not xo) (not artifficial)))
                )
            )

            (progn (format t "~%~%nedozvoljen potez, pokusajte ponovo..~%") (make-move-ai xo artifficial))))))
  )
  )
)

(defun evaluate-winner (horizontal-matrix vertical-matrix move xo)
  (cond
        ((or (and xo (< (length (cadr states)) 4))
          (and (not xo) (< (length (car states)) 4))
          (>= (check-winner-state-vertical (nth (1- (cadr move)) vertical-matrix) (cadr move) xo 0) 5)
          (>= (longest-sublist (check-winner-state-diagonal 1 horizontal-matrix (if xo 'x 'o) nil -1) 0) 5)
          (>= (longest-sublist (check-winner-state-diagonal 1 horizontal-matrix (if xo 'x 'o) nil 1) 0) 5)
        )
        (progn (show-output horizontal-matrix) (format t "~%~%Pobednik je ~A ~%~%" (if xo #\x #\o)) #+sbcl (sb-ext:quit)))

        (t (progn (show-output horizontal-matrix) t))
    )
)

(defun evaluate-winner-ai (horizontal-matrix vertical-matrix xo)
  (cond
        ((or (and xo (< (length (cadr states)) 4))
          (and (not xo) (< (length (car states)) 4))
          (equalp (apply 'min (heuristic-state-vertical vertical-matrix 0 xo )) 0)
          (>= (longest-sublist (check-winner-state-diagonal 1 horizontal-matrix (if xo 'x 'o) nil -1) 0) 5)
          (>= (longest-sublist (check-winner-state-diagonal 1 horizontal-matrix (if xo 'x 'o) nil 1) 0) 5)
         )
        t
        )
        (t nil)
    )
)

(defun validate-users-move (current move horizontal vertical xo)
  (cond
    ((or (and (not (equal (cadr current) (cadr move))) (validate-state current move (generate-states horizontal 1 xo)))
            (and (not (equal (car current) (car move))) (validate-state (list (cadr current) (car current)) (list (cadr move) (car move)) (generate-states vertical 1 xo))))
     t
        )
        (t nil)
  )
)

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

; parametri su states/states-vertical vraca listu ((new states) (new states-vertical)) :: integrisano u generator stanja (make-all-states)
(defun check-sandwich (states-ptr states-vertical-ptr move xo)
  (let* (
         (to-delete-horizontal (check-row-sandwich (list (extract-row-column (car states-ptr) (car move)) (extract-row-column (cadr states-ptr) (car move))) xo move))
         (to-delete-vertical (check-column-sandwich (list (extract-row-column (car states-vertical-ptr) (cadr move)) (extract-row-column (cadr states-vertical-ptr) (cadr move))) xo (list (cadr move) (car move))))
        )
     (list
      (list
       (remove-from-states (remove-from-states (car states-ptr) to-delete-horizontal) (inverse-all to-delete-vertical))
       (remove-from-states (remove-from-states (cadr states-ptr) to-delete-horizontal) (inverse-all to-delete-vertical))
      )
      (list
       (remove-from-states (remove-from-states (car states-vertical-ptr) to-delete-vertical) (inverse-all to-delete-horizontal))
       (remove-from-states (remove-from-states (cadr states-vertical-ptr) to-delete-vertical) (inverse-all to-delete-horizontal))
      )
     )
   )
)

(defun extract-row-column (states-ptr num)
  (cond
   ((null states-ptr) nil)
   ((< (caar states-ptr) num) (extract-row-column (cdr states-ptr) num))
   ((> (caar states-ptr) num) nil)
   (t (cons (car states-ptr) (extract-row-column (cdr states-ptr) num)))
  )
)

; sledece dve funkcije vracaju elemente koji treba da budu obrisani (x y)
(defun check-row-sandwich (states-ptr xo move)
  (let*
      (
       (player (if xo (car states-ptr) (cadr states-ptr)))
       (opponent (if xo (cadr states-ptr) (car states-ptr)))
      )
    (to-remove player opponent move)
  )
)

(defun check-column-sandwich (states-vertical-ptr xo move)
  (let*
      (
       (player (if xo (car states-vertical-ptr) (cadr states-vertical-ptr)))
       (opponent (if xo (cadr states-vertical-ptr) (car states-vertical-ptr)))
      )
    (to-remove player opponent move)
  )
)

(defun to-remove (player opponent move)
  (let*
      (
       (left-bound (car player))
       (right-bound (nth 1 player))
      )
    (cond
     ((null player) nil)
     ((and (not (null right-bound)) (not (equalp move left-bound)) (not (equalp move right-bound))) (append nil (to-remove (cdr player) opponent move)))
     (t (append (in-between left-bound right-bound (list (car left-bound) (1+ (cadr left-bound))) nil opponent) (to-remove (cdr player) opponent move)))
    )
  )
)

(defun in-between (left-bound right-bound current sublist opponent)
  (cond
   ((equalp current right-bound) sublist)
   ((not (member current opponent :test 'equal)) nil)
   (t (in-between left-bound right-bound (list (car current) (1+ (cadr current))) (cons current sublist) opponent))
  )
)

(defun remove-from-states (states-ptr elto-remove)
  (cond
   ((null elto-remove) states-ptr)
   (t (remove-from-states (remove (car elto-remove) states-ptr :test 'equal) (cdr elto-remove)))
  )
)

(defun inverse-all (uninversed)
  (cond
   ((null uninversed) nil)
   (t (cons (append (cdar uninversed) (list (caar uninversed))) (inverse-all (cdr uninversed))))
  )
)

(defun check-winner-state-horizontal (coded-row rownum xo counter) ; rownum za broj vrste | coded-row (nth rownum-1 horizontal-matrix)
  (cond
   ((null coded-row) counter)
   ((and xo (<= rownum 2)) counter)
   ((and (not xo) (> rownum (- dimension 2))) counter)
   ((equalp counter 5) counter)
   ((and (listp (car coded-row)) (equalp (cadar coded-row) (if xo 'x 'o))) (check-winner-state-horizontal (cdr coded-row) rownum xo (1+ counter)))
   (t (check-winner-state-horizontal (cdr coded-row) rownum xo 0))
  )
)

(defun altern-state-sandwich (coded-row rownum xo counter vertical-bool prev)
  (cond
   ((null coded-row) counter)
   ((and xo (not vertical-bool) (< rownum 2)) counter)
   ((and (not xo) (>= rownum (- dimension 2))) counter)
   ((or
      (and
        (listp (car coded-row))
        (equalp (cadar coded-row) (if xo 'x 'o))
        (listp (cadr coded-row))
        (equalp (cadadr coded-row) (if xo 'o 'x))
      )
      (and
        (listp (car coded-row))
        (equalp (cadar coded-row) (if xo 'x 'o))
        (listp prev)
        (equalp prev (if xo 'o 'x))
      )
    )
      (if vertical-bool
        (altern-state-sandwich (cdr coded-row) (1+ rownum) xo (1+ counter) vertical-bool (car coded-row))
        (altern-state-sandwich (cdr coded-row) rownum xo (1+ counter) vertical-bool (car coded-row))
      )
   )
   (t (if vertical-bool
        (altern-state-sandwich (cdr coded-row) (1+ rownum) xo counter vertical-bool (car coded-row))
        (altern-state-sandwich (cdr coded-row) rownum xo counter vertical-bool (car coded-row))
      )
   )
  )
)

(defun list-to-heuristic (list level multiplier result)
  (cond
    ((>= level 4) (+ result (* 0.2 (count 2 list))))
    ((equalp 1000 (* multiplier (count level list))) 5000)
    (t (+ result (* multiplier (count level list)) (list-to-heuristic  list (+ level 1) (/ multiplier 4) result)))
    )
  )

(defun non-zero-inlist (list count)
  (cond
    ((null list) count)
    ((zerop (car list)) (non-zero-inlist (cdr list) count))
    (t (non-zero-inlist (cdr list) (1+ count)))
  )
)

;; prosledjuju se nekodirane matrice i t/f, vraca se vrednost, moze se izbaciti t/f
(defun heuristic-value (states-hor states-vert xo)
(let ((coded-horizontal (states-to-matrix 1 dimension states-hor))
      (coded-vertical (states-to-matrix 1 dimension states-vert))
      (opponent-length (if xo (length (cadr states-hor)) (length (car states-hor))))
     )
    (+
       (list-to-heuristic (heuristic-state-vertical coded-vertical 0 xo ) 0 1000 0)
       (list-to-heuristic (heuristic-value-diagonal (check-winner-state-diagonal 1 coded-horizontal (if xo 'x 'o) nil 1) nil)  0 1000 0)
       (list-to-heuristic (heuristic-value-diagonal (check-winner-state-diagonal 1 coded-horizontal (if xo 'x 'o) nil -1) nil) 0 1000 0)
       (cond
          ((<= opponent-length 4) 5000)
          ((<= opponent-length 5) 400)
          ((<= opponent-length 6) 350)
          ((<= opponent-length 7) 300)
          ((<= opponent-length 8) 280)
          ((<= opponent-length 9) 260)
          ((<= opponent-length 10) 240)
          ((<= opponent-length 11) 220)
          ((<= opponent-length 12) 200)
          ((<= opponent-length 13) 180)
          ((<= opponent-length 14) 160)
          ((<= opponent-length 15) 140)
          ((<= opponent-length 16) 120)
          ((<= opponent-length 17) 100)
          ((<= opponent-length 18) 60)
          ((<= opponent-length 19) 40)
          (t 0)
       )
    )
  )
)

; (heuristic-state-sandwich (states-to-matrix 1 dimension states) 0 xo nil)
; (heuristic-state-sandwich (states-to-matrix 1 dimension states-vertical) 0 xo t)
; vertical bool: t za nalazanje potenijalnih vertikalnih sendvica
; povratna vrednost: lista, svaki elemenat evaulira stanje blizu sendvica, odnosno na koliko mesta ima susednih figura sa protivnickim; za svaku vrstu matrice (0... dimension-1)

  (defun heuristic-state-sandwich (row-matrix rownum xo vertical-bool prev)
  (if (null row-matrix) nil
    (cons
      (altern-state-sandwich (car row-matrix) rownum xo 0 vertical-bool prev)
      (heuristic-state-sandwich (cdr row-matrix) (+ rownum 1) xo vertical-bool (car row-matrix))
    )
  )
)

(defun altern-state-horizontal (coded-row rownum xo counter missing)
  (cond
   ((null coded-row) missing)
   ((and xo (< rownum 2)) missing)
   ((and (not xo) (>= rownum (- dimension 2))) missing)
   ((equalp counter 5) 0)
   ((and (listp (car coded-row)) (equalp (cadar coded-row) (if xo 'x 'o)))
  (if (> missing (- 4 counter))
      (altern-state-horizontal (cdr coded-row) rownum xo (1+ counter) (- 4 counter))
      (altern-state-horizontal (cdr coded-row) rownum xo (1+ counter) missing)
    )
    )
   (t (altern-state-horizontal (cdr coded-row) rownum xo 0 missing))
  )
)

; (heuristic-state-horizontal (states-to-matrix 1 dimension states) 0 xo)
; povratna vrednost: lista, svaki elemenat predstavlja broj figura koje fale do povezivanja 5 uzastopnih; za svaku vrstu matrice (0... dimension-1)

(defun heuristic-state-horizontal (row-matrix rownum xo )
  (if (null row-matrix) nil
    (cons
      (altern-state-horizontal (car row-matrix) rownum xo 0 5)
      (heuristic-state-horizontal (cdr row-matrix) (+ rownum 1) xo)
    )
  )
)

(defun check-winner-state-vertical (coded-column rownum xo counter) ; rownum za broj vrste i uvek se prosledjuje 1 i inkrementira se kroz funkciju
  (cond
   ((null coded-column) counter)
   ((and (not xo) (> rownum (- dimension 2))) counter)
   ((equalp counter 5) counter)
   ((and (listp (car coded-column)) (equalp (cadar coded-column) (if xo 'x 'o)) (or (and xo (> rownum 2)) (and (not xo) (<= rownum (- dimension 2))))) (check-winner-state-vertical (cdr coded-column) (1+ rownum) xo (1+ counter)))
   ((listp (car coded-column)) (check-winner-state-vertical (cdr coded-column) (1+ rownum) xo 0))
   (t (check-winner-state-vertical (cdr coded-column) (+ rownum (car coded-column)) xo 0))
  )
)

; (heuristic-state-vertical (states-to-matrix 1 dimension states-vertical) 0 xo)
; povratna vrednost: lista, svaki elemenat predstavlja broj figura koje fale do povezivanja 5 uzastopnih; za svaku kolonu matrice (0... dimension-1)

(defun heuristic-state-vertical (column-matrix columnum xo)
  (if (null column-matrix) nil
    (cons
      (altern-state-vertical (car column-matrix) columnum xo 0 5)
      (heuristic-state-vertical (cdr column-matrix) 0 xo)
    )
  )
)

(defun altern-state-vertical (coded-column rownum xo counter missing) ; rownum za broj vrste i uvek se prosledjuje 1 i inkrementira se kroz funkciju
  (cond
   ((null coded-column) missing)
   ((and (not xo) (>= rownum (- dimension 2))) missing)
   ((equalp counter 5) 0)
   ((and (listp (car coded-column)) (equalp (cadar coded-column) (if xo 'x 'o)) (or (and xo (>= rownum 2)) (and (not xo) (< rownum (- dimension 2)))))
  (if (> missing (- 4 counter))
    (altern-state-vertical (cdr coded-column) (1+ rownum) xo (1+ counter) (- 4 counter))
    (altern-state-vertical (cdr coded-column) (1+ rownum) xo (1+ counter) missing)
    )
   )
   ((listp (car coded-column)) (altern-state-vertical (cdr coded-column) (1+ rownum) xo 0 missing))
   (t (altern-state-vertical (cdr coded-column) (+ rownum (car coded-column)) xo 0 missing))
  )
)

(defun check-winner-state-diagonal (lvl encoded-list xo res lr)
    (cond
      ((null encoded-list) res)
      (t (check-winner-state-diagonal (+ lvl 1) (cdr encoded-list) xo (check-row-for-diagonal lvl
          (cond
            ((or(and (equalp xo 'x) (<= lvl 2)) (and (equalp xo 'o) (>= lvl (- dimension 1)))) nil)
            (t (car encoded-list))) xo res lr) lr))
      )
  )

;;(heuristic-value-diagonal (check-winner-state-diagonal 1 coded-horizontal 'x  nil 1) nil)
(defun heuristic-value-diagonal (diagonals result)
  (cond
    ((null diagonals) result)
    (t (cons
         (append result (- 5 (length (car diagonals))))
         (heuristic-value-diagonal (cdr diagonals) result)))
       )
    )

(defun longest-sublist (all longest)
  (cond
    ((null all) longest)
    ((> (length (car all)) longest) (longest-sublist (cdr all) (length (car all))))
    (t (longest-sublist (cdr all) longest))
    )
  )

(defun remove-atoms (lvl list res)
  (cond
    ((null list) (reverse res))
    ((and (atom (caar list))(equalp (caar list) lvl) ) (remove-atoms lvl (cdr list) res))
    (t (remove-atoms lvl (cdr list) (cond
                                      ((null res) (list (car list)))
                                      (t(cons (car list) res )))))
    )
  )

(defun check-row-for-diagonal (lvl row xo current lr)
  (cond
    ((null row) (remove-atoms (- lvl 1) current NIL))
    ((listp (encode-element (car row) xo)) (check-row-for-diagonal lvl (cdr row) xo (check-if-element-diagonal (list lvl (caar row)) current NIL lr xo) lr))
     (t (check-row-for-diagonal lvl (cdr row) xo current lr))
     )
    )

(defun check-if-not-addable-diagonal (element lr xo)
  (let*
      ((sum (+ (car element) (cadr element))))
    (cond
      ((equalp lr -1) (cond
                        ((equalp xo 'x) (cond
                                          ((or (< sum 8) (> sum (- (* dimension 2) 4))) NIL )
                                          (t T)
                                          ))
                        (t (cond
                             ((or (< sum 6) (> sum (- (* dimension 2) 6))) NIL)
                             (t T)
                             ))
                        )
       )
      (t (cond
           ((equalp xo 'x) (cond
                             ((or (and (>= (car element) 2 ) (<=  (car element) 6) (>= (- (cadr element) (car element)) (- dimension 6) ))
                                  (and (>= (cadr element) 1) (<= (cadr element) 4) (>= (- (car element) (cadr element)) (- dimension 4)))) NIL)
                             (t T)
                             ))
           (t (cond
                ((or (and (>= (car element) 2) (<= (car element) 6) (>= (- (cadr element) (car element)) (- dimension 4)))
                     (and (>= (cadr element) 1) (<= (cadr element) 4) (>= (- (car element) (cadr element)) (- dimension 6)))) NIL)
                (t T)
                )) ))
      ))
  )

(defun check-if-element-diagonal (element current res lr xo)
  (cond
    ((null  (check-if-not-addable-diagonal element lr xo))
     (cond
       ((null current) res)
       (t current)))
  (t (cond
    ((null current)
         (cond
           ((null res) (list element))
           (t (append res (list element)))
           ))
    (t (let* ((value (car current)))
         (cond
           ((equalp value (check-if-appends element value lr)) (check-if-element-diagonal element (cdr current) (cond
                                                                                                               ((null res) (list (car current)))
                                                                                                               (t(append res (list(car current))))) lr xo))
           (t  (append res (list(check-if-appends element value lr)) (cdr current)) )
           )))))))

(defun check-if-appends (element element-or-atom lr)
  (let*
      ((value (car (last element-or-atom))))
    (cond
      ((listp value)(cond
                      ((and (equalp (cadr element) (+ (cadr value) lr)) (cond ((equalp lr -1) T)
                                                                              (t (equalp(car element) (+ (car value) lr))))) (append element-or-atom (list element)))
                      (t element-or-atom)))
      ((and (equalp (cadr element) (+ (cadr element-or-atom) lr)) (cond
                                                                    ((equalp lr -1) T)
                                                                    (t (equalp(car element) (+ (car element-or-atom) lr))))) (list element-or-atom element))
      (t element-or-atom))))

;; implementiranje alfa beta algoritma

 (defun max-value (state-par alpha beta depth xo)
  (let* ((heuristic-max (heuristic-value (car state-par) (cadr state-par) xo)))
    (cond
      ((>= heuristic-max 5000) 5000)
      (t  (cond
            ((zerop depth) (- heuristic-max (heuristic-value (car state-par) (cadr state-par) (not xo))))
            (t (let
                   ((quit-flag NIL))
                 (progn
                   (loop for x in (merge-all-states (states-to-matrix 1 dimension (car state-par)) (states-to-matrix 1 dimension (cadr state-par)) (car state-par) (cadr state-par) xo ) until quit-flag
                         do (let* ((new-alpha (min-value x alpha beta (- depth 1) (not xo))))
                              (if (< alpha new-alpha) (setf alpha new-alpha)))
                            (when (>= alpha beta) (setq quit-flag T)))
           (cond
             ((null quit-flag) alpha)
             (t beta)
             )))))))))

(defun min-value (state-par alpha beta depth xo)
(let* ((heuristic-min (heuristic-value (car state-par) (cadr state-par) xo)))
  (cond
    ((>= heuristic-min 5000) -5000)
  (t (cond
    ((zerop depth) (- (heuristic-value (car state-par) (cadr state-par) (not xo)) (heuristic-value (car state-par) (cadr state-par) xo)))
    (t (let
        ((quit-flag NIL))
      (progn
      (loop for x in (merge-all-states (states-to-matrix 1 dimension (car state-par)) (states-to-matrix 1 dimension (cadr state-par)) (car state-par) (cadr state-par) xo ) until quit-flag
            do (let* ((new-beta (max-value x alpha beta (- depth 1) (not xo))))
                 (if (> beta new-beta) (setf beta new-beta)))
               (when (>= alpha beta) (setq quit-flag T)))
      (cond
        ((null quit-flag) beta)
        (t alpha)
        )))))))))

(defun alpha-beta (state-par alpha beta depth xo)
  (cond
    ((zerop depth) (heuristic-value (car state-par) (cadr state-par) xo))
    (t (let
           ((quit-flag NIL) (best-move NIL) (is-terminal NIL) )
         (progn
           (loop for x in (merge-all-states (states-to-matrix 1 dimension (car state-par)) (states-to-matrix 1 dimension (cadr state-par)) (car state-par) (cadr state-par) xo ) until quit-flag
                 do (cond
                      ((not (null (evaluate-winner-ai (states-to-matrix 1 dimension (car x)) (states-to-matrix 1 dimension (cadr x)) xo))) (progn (setf is-terminal t) (setf best-move x) (setf quit-flag T)))
                    (t (let* ((new-alpha (min-value x alpha beta (- depth 1) (not xo))))
                      (if (< alpha new-alpha) (progn (setf alpha  new-alpha) (setf best-move x))))))
                    (when (>= alpha beta) (setq quit-flag T))
                 )
         (cond
           ((null quit-flag) best-move)
           ((not (null is-terminal)) best-move)
           (t best-move)
           ))))
    )
)


(defun merge-all-states (horizontal-matrix vertical-matrix states-h states-v xo)
    (let* ((horizontal (generate-states horizontal-matrix 1 xo)) (vertical (generate-states vertical-matrix 1 xo)))

      (cond
        ((null vertical) (make-all-states states-h states-v horizontal xo nil))
        ((null horizontal) (make-all-states states-h states-v vertical xo t))
        (t (append
          (make-all-states states-h states-v horizontal xo nil)
          (make-all-states states-h states-v vertical xo t)
      ))
      )
    )
)

(defun make-all-states (states-h states-v all-states xo invert)
  (cond
   ((null all-states) nil)
   ((not (null (cadar all-states))) (append (make-states states-h states-v (caaar all-states) (cadaar all-states) (cadar all-states) xo invert) (make-all-states states-h states-v (cdr all-states) xo invert)))
   (t (make-all-states states-h states-v (cdr all-states) xo invert))
  )
)

(defun make-states (states-h states-v x y possible xo invert)
  (cond
   ((null possible) nil)
   ((atom (car possible)) (list (make-state states-h states-v x y (car possible) (cadr possible) xo invert)))
   (t (cons (make-state states-h states-v x y (caar possible) (cadar possible) xo invert) (make-states states-h states-v x y (cdr possible) xo invert)))
  )
)

(defun make-state (states-h states-v x y x-new y-new xo invert)
  (cond
   ((and (not invert) xo) (progn
         (check-sandwich
         (list (insert-state x-new y-new (remove-state x y (car states-h))) (cadr states-h))
         (list (insert-state y-new x-new (remove-state y x (car states-v))) (cadr states-v))
         (list x-new y-new)
         t
         )))
   ((and (not invert) (not xo)) (progn
         (check-sandwich
         (list (car states-h) (insert-state x-new y-new (remove-state x y (cadr states-h))))
         (list (car states-v) (insert-state y-new x-new (remove-state y x (cadr states-v))))
         (list x-new y-new)
         nil
         )))
   ((and invert xo) (progn
         (check-sandwich
         (list (insert-state y-new x-new (remove-state y x (car states-h))) (cadr states-h))
         (list (insert-state x-new y-new (remove-state x y (car states-v))) (cadr states-v))
         (list y-new x-new)
         t
         )))
   ((and invert (not xo)) (progn
         (check-sandwich
         (list (car states-h) (insert-state y-new x-new (remove-state y x (cadr states-h))))
         (list (car states-v) (insert-state x-new y-new (remove-state x y (cadr states-v))))
         (list y-new x-new)
         nil
         )))
  )
)

(defun validate-state (source destination all-states)
  (cond
   ((null all-states) nil)
   ((and (equalp source (caar all-states)) (or (member destination (cadar all-states) :test 'equal) (equal destination (cadar all-states)))) t)
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
                      ((listp lst) (cond
                                     ((and (not(null seclst)) (listp seclst)) (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl seclst 0 T T) (append-moves-for-row lvl lst value NIL T))))
                       (t (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl lst value NIL T))))))
                      ((zerop lst)(cond
                                    ((and (not(null seclst)) (listp seclst))(generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl seclst 0 T T))))
                                    (t (generate-moves-for-row lvl lst value xo (cdr row) res))))
                      ))
      ;; ako je value lista, a pethodi mu slobodno mesto
      ((and(atom lst) (not (zerop lst))) (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl value lst T T))))
      ((and (numberp seclst)(not (zerop seclst))) (generate-moves-for-row lvl lst value xo (cdr row) (append res (append-moves-for-row lvl value 0 T nil))))
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

(defun append-moves-for-row (lvl el size prev forward)
    (list(cons (list lvl (car el)) (list (cond
                                           ((zerop size) (cond ((not(null forward))(list lvl (+ (car el) 2)))
                                                               (t (list lvl (- (car el) 2)))))
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
         (let* ((new-state (check-sandwich states states-vertical (list x-new y-new) t)))
           (progn
             (setq states (car new-state))
             (setq states-vertical (cadr new-state))
           )
         )
         ))
   ((not xo) (progn
         (setq states-vertical (list (car states-vertical) (insert-state y-new x-new (remove-state y x (cadr states-vertical)))))
         (setq states (list (car states) (insert-state x-new y-new (remove-state x y (cadr states)))))
         (let* ((new-state (check-sandwich states states-vertical (list x-new y-new) nil)))
           (progn
             (setq states (car new-state))
             (setq states-vertical (cadr new-state))
           )
         )
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
