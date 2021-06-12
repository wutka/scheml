(load "lists.scm")

(define (int->list n) (list n))

(define (make-set n)
  (if (= n 0) (range 1 9)
      (list n)))

(define (in-set n s)
  (if (null? s) #f
      (if (= (head s) n) #t
          (in-set n (tail s)))))

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

(define (print-sudoku-row r)
  (printf "%d %d %d   %d %d %d   %d %d %d\n"
          (head (nth 0 r))
          (head (nth 1 r))
          (head (nth 2 r))
          (head (nth 3 r))
          (head (nth 4 r))
          (head (nth 5 r))
          (head (nth 6 r))
          (head (nth 7 r))
          (head (nth 8 r))))

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

(define (is-unique n sets)
  (letrec ((is-unique' 
             (lambda (count sets)
               (if (or (> count 1) (null? sets)) (= 1 count)
                   (if (member? n (head sets))
                       (is-unique' (+ count 1) (tail sets))
                       (is-unique' count (tail sets)))))))
    (is-unique' 0 sets)))

(define (find-unique set sets)
  (letrec ((find-unique'
             (lambda (n)
               (if (> n 9) set
                   (if (is-unique n sets) (list n)
                       (find-unique' (+ n 1)))))))
    (find-unique' 1)))

(define (search-for-unique sud)
  (map (lambda (i)
         (let ((cell (nth i sud)))
           (if (> (length cell) 1) (find-unique (list 999)
                                                (append (row (row-num i) sud)
                                                        (append (col (col-num i) sud)
                                                                (box (box-num i) sud))))
               (list 999))))
       (range 0 80)))

(define (remove-from-set other-set set)
  (if (= (length set) 1) set
    (if (or (> (length other-set) 1) (null? other-set)) set
        (remove (head other-set) set))))

(define (reduce-cell sud n-set n)
  (fold remove-from-set n-set (append (row (row-num n) sud) 
                                      (append (col (col-num n) sud)
                                              (box (box-num n) sud)))))

(define (reduce-cells sud)
  (letrec ((reduce-cells' (lambda (n sud-rest acc)
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
               (if (= n pos) (append-back acc (cons val (tail sud-rest)))
                   (set-cell' (+ n 1) (tail sud-rest) (cons (head sud-rest) acc))))))
    (set-cell' 0 sud nil)))

(define (is-set-correct vals)
  (= (fold (lambda (n v) (+ (<< 1 n) v)) 0 vals) 1022))

(define (is-pos-correct n sud)
  (and (is-set-correct (map head (row (row-num n) sud)))
      (and (is-set-correct (map head (col (col-num n) sud)))
           (is-set-correct (map head (box (box-num n) sud))))))

(define (correct sud) 
  (letrec ((correct' 
            (lambda (n)
              (if (> n 80) #t
                  (if (is-pos-correct n sud) (correct' (+ n 1))
                      #f)))))
    (correct' 0)))

;;(define (done sud)
;;  (if (all (= 1) (map length sud))
;;       (correct sud)
;;       #f))
(define (done sud)
  (all (= 1) (map length sud)))

(define (find-cells-with-length n sud acc)
  (letrec ((find-cell-with-length' 
             (lambda (pos sud acc)
                (if (null? sud) acc
                    (if (= (length (head sud)) n) 
                        (find-cell-with-length' (+ pos 1) (tail sud) (cons pos acc))
                        (find-cell-with-length' (+ pos 1) (tail sud) acc))))))
    (find-cell-with-length' 0 sud acc)))

(define (make-sorted-try-list sud)
  (letrec ((make-sorted-try-list' 
             (lambda (n acc)
               (if (< n 2) acc
                   (make-sorted-try-list' (- n 1)
                                          (find-cells-with-length n sud acc))))))
    (make-sorted-try-list' 9 nil)))

(define (try-solve-val-at-pos pos val sud try-func try-list)
  (let ((reduced (reduce (set-cell (list val) pos sud))))
    (if (done reduced) reduced
        (if (null? try-list) nil
            (try-func (tail try-list) reduced)))))

(define (try-solve-vals pos vals sud try-func try-list)
  (if (null? vals) nil
    (let ((tried (try-solve-val-at-pos pos (head vals) sud try-func try-list)))
      (if (null? tried)
          (try-solve-vals pos (tail vals) sud try-func try-list)
          tried))))

(define (solve-list try-list sud)
  (if (null? try-list) nil
      (let* ((n (head try-list))
             (vals (nth n sud)))
        (if (= (length vals) 1) (solve-list (tail try-list) sud)
            (try-solve-vals n vals sud solve-list (tail try-list))))))

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

(define bar (map make-set example-sudoku))
