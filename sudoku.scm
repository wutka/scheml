(load "lists.scm")

(define (int->list n) (list n))

(type square (Fixed int) (Unfixed (cons int)))

(define (make-set n)
  (if (= n 0) (Unfixed (range 1 9))
      (Fixed n)))

(define (row n sud) 
  (take 9 (drop (* 9 n) sud)))


(define (col n sud)
  (letrec ((col' (lambda (sud)
                    (if (null? sud) nil
                        (cons (head sud) (col' (drop 9 sud)))))))
    (col' (drop n sud))))

(define (box n sud)
  (letrec ((box' (lambda (n sud)
                 (if (<= n 0) nil
                     (append (take 3 sud) (box' (- n 1) (drop 9 sud)))))))
    (box' 3 (drop (+ (* 27 (/ n 3))
                      (* 3 (% n 3))) sud))))

(define (row-num idx)
  (/ idx 9))

(define (col-num idx)
  (% idx 9))

(define (box-num idx)
  (+ (* 3 (/ idx 27))
     (/ (% idx 9) 3)))

(define (sudoku-char square)
  (match square
     ((Fixed n) (int->char (+ 48 n)))
     ((Unfixed _) #\?)))

(define (print-sudoku-row r)
  (printf "%c %c %c   %c %c %c   %c %c %c\n"
          (sudoku-char (nth 0 r))
          (sudoku-char (nth 1 r))
          (sudoku-char (nth 2 r))
          (sudoku-char (nth 3 r))
          (sudoku-char (nth 4 r))
          (sudoku-char (nth 5 r))
          (sudoku-char (nth 6 r))
          (sudoku-char (nth 7 r))
          (sudoku-char (nth 8 r))))

(define (print-sudoku sud)
  (print-sudoku-row (row 0 sud))
  (print-sudoku-row (row 1 sud))
  (print-sudoku-row (row 2 sud))
  (printf "\n")
  (print-sudoku-row (row 3 sud))
  (print-sudoku-row (row 4 sud))
  (print-sudoku-row (row 5 sud))
  (printf "\n")
  (print-sudoku-row (row 6 sud))
  (print-sudoku-row (row 7 sud))
  (print-sudoku-row (row 8 sud)))

(define (remove-from-set other-set set)
  (match set
         ((Fixed _) set)
         ((Unfixed available)
          (match other-set
                 ((Fixed n) (Unfixed (remove n available)))
                 ((Unfixed _) set)))))

(define (occurs-free n square)
  (match square
     ((Fixed f) #f)
     ((Unfixed available) (member? n available))))

(define (count-occurrences n square-list acc)
  (if (null? square-list) acc
      (if (occurs-free n (head square-list))
          (count-occurrences n (tail square-list) (+ acc 1))
          (count-occurrences n (tail square-list) acc))))

(define (try-fix-list sq vals square-list)
  (if (null? vals) sq
      (if (= 1 (count-occurrences (head vals) square-list 0))
          (Fixed (head vals))
          (try-fix-list sq (tail vals) square-list))))

(define (try-fix sq square-list)
  (match sq
     ((Fixed _) sq)
     ((Unfixed available) (try-fix-list sq available square-list))))

(define (reduce-cell sud n-set n)
  (let* ((row-squares (row (row-num n) sud))
         (col-squares (col (col-num n) sud))
         (box-squares (box (box-num n) sud))
         (reduced (fold remove-from-set n-set 
                        (append row-squares
                                (append col-squares box-squares)))))
    (try-fix (try-fix (try-fix reduced row-squares) col-squares) box-squares)))


(define (reduce-cells sud)
  (letrec ((reduce-cells' 
             (lambda (n sud-rest acc)
               (if (< n 0) acc
                   (reduce-cells' (- n 1) (tail sud-rest)
                                  (cons (reduce-cell sud (head sud-rest) n) acc))))))
    (reduce-cells' 80 (reverse sud) nil)))
                                       
(define (reduce sud)
  (let ((new-sud (reduce-cells sud)))
    (if (equals? new-sud sud) new-sud
        (reduce new-sud))))

(define (set-cell val pos sud)
  (letrec ((append-back 
             (lambda (front back)
               (if (null? front) back
                   (append-back (tail front) (cons (head front) back)))))
           (set-cell' 
             (lambda (n sud-rest acc)
               (if (= n pos) (append-back acc (cons (Fixed val) (tail sud-rest)))
                   (set-cell' (+ n 1) (tail sud-rest) (cons (head sud-rest) acc))))))
    (set-cell' 0 sud nil)))

(define (set-correct? vals)
  (= (fold (lambda (val n) 
             (match val
                    ((Fixed v) (+ (<< 1 v) n))
                    ((Unfixed _) 0)))
             0 vals) 1022))

(define (pos-correct? n sud)
  (and (set-correct? (row (row-num n) sud))
      (and (set-correct? (col (col-num n) sud))
           (set-correct? (box (box-num n) sud)))))

(define (correct? sud) 
  (letrec ((correct' 
            (lambda (n)
              (if (> n 80) #t
                  (if (pos-correct? n sud) (correct' (+ n 1))
                      #f)))))
    (correct' 0)))

;;(define (done sud)
;;  (if (all (= 1) (map length sud))
;;       (correct sud)
;;       #f))

(define (fixed? square)
  (match square
     ((Fixed _) #t)
     ((Unfixed _) #f)))

(define (done sud)
  (if (all fixed? sud)
      (correct? sud)
      #f))

(define (num-available square)
  (match square
         ((Fixed _) 1)
         ((Unfixed l) (length l))))

(define (get-available square)
  (match square
     ((Fixed _) (fail "Tried to get available from fixed square"))
     ((Unfixed available) available)))

(define (failed? sud)
  (some (= 0) (map num-available sud)))

(define (find-cells-with-length n sud acc)
  (letrec ((find-cell-with-length' 
             (lambda (pos sud acc)
                (if (null? sud) acc
                    (if (= (num-available (head sud)) n) 
                        (find-cell-with-length' (+ pos 1) (tail sud) (cons pos acc))
                        (find-cell-with-length' (+ pos 1) (tail sud) acc))))))
    (find-cell-with-length' 0 sud acc)))

(define (make-sorted-try-list sud)
  (letrec ((make-sorted-try-list' 
             (lambda (n acc)
               (if (< n 2) acc
                   (make-sorted-try-list' 
                     (- n 1)
                     (find-cells-with-length n sud acc))))))
    (make-sorted-try-list' 9 nil)))

(define (try-solve-val-at-pos pos val sud try-func try-list)
  (let ((reduced (reduce (set-cell val pos sud))))
    (if (done reduced) reduced
        (if (failed? reduced) nil
            (if (null? try-list) nil
                (try-func (tail try-list) reduced))))))

(define (try-solve-vals pos available sud try-func try-list)
   (if (null? available) nil
       (let ((tried (try-solve-val-at-pos pos (head available) sud try-func try-list)))
        (if (null? tried)
           (try-solve-vals pos (tail available) sud try-func try-list)
           tried))))

(define (solve-list try-list sud)
  (if (null? try-list) nil
      (let* ((n (head try-list))
             (sq (nth n sud)))
        (if (fixed? sq) (solve-list (tail try-list) sud)
            (try-solve-vals n (get-available sq) sud solve-list (tail try-list))))))

(define (solve-sudoku sud-puzzle)
  (let* ((sud (map make-set sud-puzzle))
         (reduced (reduce sud)))
    (solve-list (make-sorted-try-list sud) sud)))

(define example-sudoku (list
 8 5 0  0 0 2  4 0 0
 7 2 0  0 0 0  0 0 9
 0 0 4  0 0 0  0 0 0

 0 0 0  1 0 7  0 0 2
 3 0 5  0 0 0  9 0 0
 0 4 0  0 0 0  0 0 0

 0 0 0  0 8 0  0 7 0
 0 1 7  0 0 0  0 0 0
 0 0 0  0 3 6  0 4 0))


(define (line-to-sudoku line)
  (map make-set 
       (map (lambda (ch) (- (char->int (if (equals? ch #\.) #\0 ch)) 48)) (string->list line))))

(define (fixed-val sq)
  (match sq
    ((Fixed v) v)
    ((Unfixed _) (fail "Tried to read unfixed value"))))

(define (sudoku-to-line sudoku)
  (list->string (map (lambda (sq) (int->char (+ 48 (fixed-val sq)))) sudoku)))

(define (solve-sudoku-line line)
  (let* ((sud (line-to-sudoku line))
         (reduced (reduce sud))
         (solved (solve-list (make-sorted-try-list sud) sud)))
    (printf "Solved: %s\n" (sudoku-to-line solved))
    solved))

(define (solve-sudoku-file filename out-filename)
  (statements
    (:= lines (read-lines filename))
    (:= results (map solve-sudoku-line lines))
    (write-lines out-filename (map sudoku-to-line results))))
