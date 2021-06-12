(load "lists.scm")

;;; A sudoku square is either fixed at a particular number or is unfixed
;;; and has a list of possible numbers that can go in that square
(type square (Fixed int) (Unfixed (cons int)))

;;; Creates a square from an initial number. If the number is in the range
;;; 1-9, then the square is fixed, otherwise if it is 0, then the
;;; square is unfixed and has 1-9 as possible values
(define (make-square n)
  (if (= n 0) (Unfixed (range 1 9))
      (Fixed n)))

;;; Returns row n of a sudoku, which consists of skipping past 9*n values
;;; and then returning the next 9 values
(define (row n sud) 
  (take 9 (drop (* 9 n) sud)))


;;; Returns column n of a sudoku, which consists of skipping the first n
;;; values and then returning a list of every 9th value
(define (col n sud)
  (letrec ((col' (lambda (sud)
                    (if (null? sud) nil
                        (cons (head sud) (col' (drop 9 sud)))))))
    (col' (drop n sud))))

;;; Returns box n of a sudoku. First, we position the list to where the
;;; box starts, which is 27 * (n/3) + (3 * (n % 3). If you consider that
;;; a sudoku consists of 3 rows of 3x3 boxes, a box row contains 3*3*3 (27)
;;; elements, so skipping 27 * (n/3) elements (where n is in the range 0-2)
;;; positions us at the row where the box begins. Then, in a given row,
;;; the box in that row starts at 3 * (n % 3), so if n = 0, 0, if n = 1, then 3
;;; and if n = 6, then 6. We take those 3 values, and then jump 9 places ahead
;;; (the 9 includes the 3 we took) and take the next 3, and repeat once more
(define (box n sud)
  (letrec ((box' (lambda (n sud)
                 (if (<= n 0) nil
                     (append (take 3 sud) (box' (- n 1) (drop 9 sud)))))))
    (box' 3 (drop (+ (* 27 (/ n 3))
                      (* 3 (% n 3))) sud))))

;;; Returns the row number for a given offset in the sudoku. The offset is in
;;; the range 0-80.
(define (row-num idx)
  (/ idx 9))

;;; Returns the column number for a given offset in the sudoku. The offset is in
;;; the range 0-80.
(define (col-num idx)
  (% idx 9))

;;; Returns the box number for a given offset in the sudoku. The offset is in
;;; the range 0-80. The computation for the box number is similar to the logic
;;; in the box function above
(define (box-num idx)
  (+ (* 3 (/ idx 27))
     (/ (% idx 9) 3)))

;;; Returns each sudoku square as a character, either the digit for a fixed square
;;; or ? for a square that is unfixed
(define (square->char square)
  (match square
     ;; 48 is the ascii code for 0
     ((Fixed n) (int->char (+ 48 n)))
     ((Unfixed _) #\?)))

;;; Prints a sudoku row
(define (print-sudoku-row r)
  (printf "%c %c %c   %c %c %c   %c %c %c\n"
          (square->char (nth 0 r))
          (square->char (nth 1 r))
          (square->char (nth 2 r))
          (square->char (nth 3 r))
          (square->char (nth 4 r))
          (square->char (nth 5 r))
          (square->char (nth 6 r))
          (square->char (nth 7 r))
          (square->char (nth 8 r))))

;;; Prints the sudoku
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

;;; Returns true if a square is fixed
(define (fixed? square)
  (match square
     ((Fixed _) #t)
     ((Unfixed _) #f)))

;;; Computes whether or not a set is correct. That us, does a particular row,
;;; column, or box in the sudoku contain exactly the digits 1-9. We compute this
;;; by summing 1<<n for each value n in the set. The sum should be 1022, which
;;; corresponds to the binary 1111111110 (bit 0 is 0, and bits 1-9 are set).
(define (set-correct? vals)
  (= (fold (lambda (val n) 
             (match val
                    ((Fixed v) (+ (<< 1 v) n))
                    ((Unfixed _) 0)))
             0 vals) 1022))

;;; Returns true if every square in a set (row/col/box) is fixed
(define (set-fixed? vals)
  (all fixed? vals))

;;; Returns true if row n, col n, and box n are all correct
(define (sets-correct? sud n)
  (and (set-correct? (row n sud))
      (and (set-correct? (col n sud))
           (set-correct? (box n sud)))))

;;; Check all 9 rows, columns, and boxes to see if they are each correct
(define (correct? sud) 
  (all id (map (sets-correct? sud) (range 0 8))))

;;; If other-square is fixed and sq is unfixed, remove other-square's value
;;; from sq's list of available squares
(define (remove-from-available other-square sq)
  (match sq
         ((Fixed _) sq)
         ((Unfixed available)
          (match other-square
                 ((Fixed n) (Unfixed (remove n available)))
                 ((Unfixed _) sq)))))

;;; Is n a possible value of unfixed square sq?
(define (occurs-free? n sq)
  (match sq
     ((Fixed f) #f)
     ((Unfixed available) (member? n available))))

;;; How many times does value n occur in the available lists in a given
;;; list of squares. The acc parameter should be passed in as 0.
(define (count-occurrences n square-list acc)
  (if (null? square-list) acc
      (if (occurs-free? n (head square-list))
          (count-occurrences n (tail square-list) (+ acc 1))
          (count-occurrences n (tail square-list) acc))))

;;; In the given square, is there a value that only occurs once in the
;;; given list of squares? For example, suppose the list of squares contains
;;; the following lists of possible squares:
;;; (1 2 3 5) (1 3 5 7) (2 5) (1 2 3 5)
;;; Since 7 only occurs in one list, the that particular square can be fixed
;;; at 7 since none of the other squares can.
(define (try-fix-list sq vals square-list)
  (if (null? vals) sq
      ;;; If the value at the head of the list only occurs once in the list
      ;;; of squares (which includes this square), then fix this square at
      ;;; that number
      (if (= 1 (count-occurrences (head vals) square-list 0))
          (Fixed (head vals))
          ;;; Otherwise try the next value in this square's availability list
          (try-fix-list sq (tail vals) square-list))))

;;; Try to fix a square based on whether it contains a value that doesn't appear
;;; in any of the other available lists in the same row, column, or box
(define (try-fix sq square-list)
  (match sq
     ((Fixed _) sq)
     ((Unfixed available) (try-fix-list sq available square-list))))

;;; After we have reduced the available list of a square, if it only contains a
;;; single value, go ahead and fix it at that value.
(define (fix-if-one sq)
  (match sq
    ((Fixed _) sq)
    ((Unfixed available)
     (if (= 1 (length available)) (Fixed (head available))
         sq))))

;;; Sets square at pos in the sudoku to val
(define (set-square val pos sud)
  (replace-nth pos val sud))

;;; Reduces the available set in a given square by removing values of any fixed
;;; values in the same row, col, or box
(define (reduce-square sud sq n)
  ;;; If this square is already fixed, no need to reduce it
  (if (fixed? sq) sq
    ;;; Get a list of the items in the same row, col, or box as this square
    (let* ((row-squares (row (row-num n) sud))
         (col-squares (col (col-num n) sud))
         (box-squares (box (box-num n) sud))
         (reduced 
           ;;; Fix the result if it is a single value
           (fix-if-one
             ;;; Proceed through the list of squares in the same row, col, and box
             ;;; and reduce this square's availability by each value, resulting in
             ;;; a new square with its availability possibly changed
                    (fold remove-from-available sq 
                        (append row-squares
                                (append col-squares box-squares))))))

      ;;; After reducing the square, see if it contains a value that doesn't appear in
      ;;; the available lists in either the same row, col, or box
      (if (fixed? reduced) reduced
        (try-fix 
          (try-fix 
            (try-fix reduced row-squares) 
            col-squares) 
          box-squares)))))

;;; Reduce the squares in the sudoku, updating the sudoku each time before processing the
;;; next one to make sure any changes aren't ignored when processing the next square
(define (reduce-squares sud)
  (letrec ((reduce-squares'
             (lambda (n curr-sud)
               ;;; If we hit the end, curr-sud is the reduced sudoku
               (if (< n 0) curr-sud
                   (reduce-squares'
                     (- n 1)
                     ;;; recur with the sudoku generated by reducing square n
                     (set-square (reduce-square curr-sud (nth n sud) n) n curr-sud))))))
    ;;; Start at square 80 and work backwards (no particular reason for going backwards
    ;;; except that recurring with a value that goes down towards 0 is a good practice
    ;;; if you work with proofs in programs where it can prove that recursion terminates
    ;;; because a particular value keeps getting smaller. 
    (reduce-squares' 80 sud)))


;;; Reduces the sudoku by paring down the lists of available values based on what values
;;; in the same row, column, or box have been fixed.
(define (reduce sud)
  (let ((new-sud (reduce-squares sud)))
    (if (equals? new-sud sud) new-sud
        (reduce new-sud))))

;;; Returns true if the sudoku is completely fixed and each row, column, and box contains
;;; the digits 1-9
(define (done sud)
  (if (all fixed? sud)
      (correct? sud)
      #f))

;;; Returns the number of values available in a square
(define (num-available square)
  (match square
         ((Fixed _) 1)
         ((Unfixed l) (length l))))

;;; Returns the list of possible values for a square, this should only be called
;;; on an unfixed square.
(define (get-available square)
  (match square
     ((Fixed _) (fail "Tried to get available from fixed square"))
     ((Unfixed available) available)))

;;; If there exists an unfixed square with no possible values, this particular
;;; solution attempt has failed. It's not a bug, just that it's not a workable
;;; combination of possibilities.
(define (failed? sud)
  (some (= 0) (map num-available sud)))

;;; Find all squares that have n available digits
(define (find-squares-with-length n sud acc)
  (letrec ((find-square-with-length' 
             (lambda (pos sud acc)
                (if (null? sud) acc
                    (if (= (num-available (head sud)) n) 
                        ;;; if this square has n available digits, add it to cons
                        (find-square-with-length' (+ pos 1) (tail sud) (cons pos acc))
                        ;;; otherwise just try the next square
                        (find-square-with-length' (+ pos 1) (tail sud) acc))))))
    (find-square-with-length' 0 sud acc)))

;;; Make a "sorted" list of squares to try, starting at those with the fewest choices,
;;; because if there are only 2 choices, you have a 50% chance of getting it right on
;;; the first try. Rather than adding a list sorting algorithm, we know that the highest
;;; number of available squares is 9, and the fewest is 2 (because at 1 it will become
;;; a fixed square), so we just build the list starting from those with 9 available,
;;; then adding the 8s, and on down to 2.
(define (make-sorted-try-list sud)
  (letrec ((make-sorted-try-list' 
             (lambda (n acc)
               (if (< n 2) acc
                   (make-sorted-try-list' 
                     (- n 1)
                     (find-squares-with-length n sud acc))))))
    (make-sorted-try-list' 9 nil)))

;;; Tries to fix val at position pos in the sudoku. The try-func parameter is an
;;; alternative to making this function and the next one as nested functions
;;; inside of solve-list. The solve-list function passes a copy of itself
;;; and the next value it wants to try down through the try try-solve- functions
;;; and they then call it tail-recursively
(define (try-solve-val-at-pos pos val sud try-func try-list)
  ;;; Try fixing square pos at with value val and reduce the sudoku
  (let ((reduced (reduce (set-square (Fixed val) pos sud))))
    ;;; If we are done, return the sudoku
    (if (done reduced) reduced
        ;;; If the sudoku now has squares that have no available values left,
        ;;; return nil to indicate that this try has failed
        (if (failed? reduced) nil
            ;;; If there are no more values to try in the try-list, give up
            (if (null? try-list) nil
                ;;; Otherwise, call back to solve-list to try solving the next
                ;;; square in the list
                (try-func (tail try-list) reduced))))))

;;; Given a list of possible values, try each value, and if we get back nil, indicating
;;; that the value didn't lead to a solution, try the next vaue
(define (try-solve-vals pos available sud try-func try-list)
  ;; If there are no more values to try, this path has failed
   (if (null? available) nil
       ;; Try the next value
       (let ((tried (try-solve-val-at-pos pos (head available) sud try-func try-list)))
         ;; if that try failed, try the next value
         (if (null? tried)
           (try-solve-vals pos (tail available) sud try-func try-list)
           ;; Otherwise, if it succeeded return the success
           tried))))

;;; Solves a list of unfixed squares trying one at a time
(define (solve-list try-list sud)
;; Uncomment these to see it in action
;;  (printf "%c[2J" (int->char 27))
;;  (print-sudoku sud))
  ;; If the list is empty, give up
  (if (null? try-list) nil
      ;; Get the position of the next square to try
      (let* ((n (head try-list))
             ;; Get the next square to try
             (sq (nth n sud)))
        ;; If this square has already bene fixed, 
        (if (fixed? sq) (solve-list (tail try-list) sud)
            ;; Otherwise, try each possible value
            (try-solve-vals n (get-available sq) sud solve-list (tail try-list))))))

;; Given a sudoku that is a list of 81 numbers between 0 and 9, convert it to a
;; list of square values and try to solve it
(define (solve-sudoku sud-puzzle)
  (let* ((sud (map make-square sud-puzzle))
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


;;; The following functions support a sudoku format found on
;;; Peter Norvig's Sudoku web page: https://norvig.com/sudoku.html
;;; The sudoku is given as a line of text in a file with 81 chars, with
;;; the digits 1-9 indicating fixed positions, and . indicating unfixed ones
(define (line->sudoku line)
  (map make-square 
       (map (lambda (ch) 
              ;; Convert the char to an integer in the range 0-9, and if the
              ;; incoming char was ., convert it to '0' so the conversion
              ;; turns it into int 0.
              (-
                ;; Convert the char to an int, turning 
                (char->int
                  ;; if the char is a ., turn it into a 0
                  (if (equals? ch #\.) #\0 ch))
                ;; We subtract 48 from the char value to convert from 
                ;; ascii '0'-'9' to int 0-9
                 48)) 
            ;; Turn the line into a string of chars
            (string->list line))))

;;; Turns a fixes square value into the integer it holds
(define (fixed->int sq)
  (match sq
    ;; return the extracted value
    ((Fixed v) v)
    ;; This should only be called on a fixed value
    ((Unfixed _) (fail "Tried to read unfixed value"))))

;;; Converts a solved sudoku back into a string
(define (sudoku->line sudoku)
  ;; Convert the list of sudoku characters back to a string
  (list->string 
    (map 
      (lambda (sq) 
        ;; Turn the square value 1-9 into the char 1-9 by adding 48 (ASCII 0)
        ;; and turning that into a char
        (int->char (+ 48 (fixed->int sq))))
      ;; Do this over the whole sudoku
      sudoku)))

;;; Solves a sudoku specified by a string, returning the solution as a string
(define (solve-sudoku-line line)
  ;; Parse the line
  (let* ((sud (line->sudoku line))
         ;; Reduce the initial sudoku
         (reduced (reduce sud))
         ;; Solve it and turn the solution into a string
         (solved (solve-list (make-sorted-try-list sud) sud)))
    (printf "Solved: %s\n" (sudoku->line solved))
    solved))

;;; Reads a file of sudokus, solves each one, and then writes the solution
;;; out to another file
(define (solve-sudoku-file filename out-filename)
  (statements
    ;;; Read the incoming list of sudokus
    (:= lines (read-lines filename))
    ;;; Run the solver on each one
    (:= results (map solve-sudoku-line lines))
    ;;; Write the results to another file
    (write-lines out-filename (map sudoku->line results))))
