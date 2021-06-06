(define LATITUDE-MIN -90.0)
(define LATITUDE-MAX 90.0)
(define LONGITUDE-MIN -180.0)
(define LONGITUDE-MAX 180.0)

;;; Take the head of list l and prepend it to revved
;;; If l is empty, return revved, which should be the
;;; reversed list
(define (reverse-1 l revved)
  (if (null? l) revved
      (reverse-1 (tail l) (cons (head l) revved))))

;;; Use a tail-recursive mechanism to reverse list
(define (reverse l) (reverse-1 l nil))

(define (map f l) 
  (if (null? l) nil
      (cons (f (head l)) (map f (tail l)))))

(define (append-1 l appended)
  (if (null? l) appended
      (append-1 (tail l) (cons (head l) appended))))

(define (append a b)
  (append-1 (reverse a) b))

(define (member? a l)
  (if (null? l) #f
      (if (equals? a (head l)) #t
          (member? a (tail l)))))

(define (remove-1 a l acc)
  (if (null? l) (reverse acc)
      (if (equals? a (head l)) (append (reverse acc) (tail l))
          (remove-1 a (tail l) (cons (head l) acc)))))

(define (remove a l) (remove-1 a l nil))

(define (take n l)
  (if (null? l) nil
      (if (<= n 0) nil
          (cons (head l) (take (- n 1) (tail l))))))

;;; Compute a 1-dimensional geohash recursively
;;; The list of bits is of type "cons char", which makes it
;;; easy to convert into a string.
(define (geohash-1d-rec value-to-hash bottom top bits-of-precision
                        curr-bit bits)
  ; If curr-bit = bits-of-precision, we are done
  ; Since the list of bits is built in reverse order, return the
  ; reverse of the list
  (if (= curr-bit bits-of-precision) (reverse bits)

    ; Compute the midpoint between the bottom and top
    (let ((middle (/. (+. bottom top) 2.0)))
       (if (<. value-to-hash middle)
       ; If the value is below the mid point, the mid point is the new top
       ; and the geohash bit at this position is 0
           (geohash-1d-rec value-to-hash bottom middle bits-of-precision
                                         (+ curr-bit 1) (cons #f bits))
       ; Otherwise the mid point is the new bottom and the geohash bit
       ; at this position is 1
           (geohash-1d-rec value-to-hash middle top bits-of-precision
                                         (+ curr-bit 1) (cons #t bits))))))

;;; Calls the tail-recursive geohash-1d-rec with an initial curr-bit and empty
;;; list of bits
(define (geohash-1d value-to-hash bottom top bits-of-precision)
  (geohash-1d-rec value-to-hash bottom top bits-of-precision
                                0 nil))

;;; Merges two lists of bits together. If the first list is shorter than the second
;;; list, then the rest of the second list is ignored. If the first list is
;;; longer than the second list, one more bit is added from the first list.
(define (merge-bits l1 l2 accum)

  ; If the first list is empty, we are done, return the accumulated bits, which
  ; were accumulated in reverse order
  (if (null? l1) (reverse accum)

      ; If the second list is null, add the head of the first list onto the accum
      ; and then return the accumulated bits after reversing them
      (if (null? l2) (reverse (cons (head l1) accum))

          ; Otherwise, add the head of l2 and l1 onto the accum and recurse
          ; on the tails of the two lists. We put l1 onto accum first and then
          ; l2 because accum is built in reverse order
          (merge-bits (tail l1) (tail l2) (cons (head l2) 
                                                (cons (head l1) accum))))))

;;; Compute a 2d geohash from two values and return a list of bits
(define (geohash-2d v1 v1-bottom v1-top v2 v2-bottom v2-top bits-of-precision)
  ; Extra-bit is 1 if bits-of-precision is odd, so that the geohash of v1 is
  ; one bit longer than that of v2
  (let* ((extra-bit (mod bits-of-precision 2))

         ; bop-half is the number of bits in each half, not counting extra-bit
         (bop-half (/ bits-of-precision 2))

         ; compute the geohash of the first value
         (first-bits (geohash-1d v1 v1-bottom v1-top (+ bop-half extra-bit)))

         ; compute the geohash of the second value
         (second-bits (geohash-1d v2 v2-bottom v2-top bop-half)))

    ; merge the results
    (merge-bits first-bits second-bits nil)))

;;; Convert the list of true/false to a list of character 1s and 0s, then
;;; convert the list to a string
(define (geohash->string hash)
  (list->string (map (lambda (bit) (if bit #\1 #\0)) hash)))

;;; Returns a geohash as a list of bits
(define (geohash lat lon bits-of-precision)
  (geohash-2d lat LATITUDE-MIN LATITUDE-MAX
              lon LONGITUDE-MIN LONGITUDE-MAX
              bits-of-precision))

(type tree-node ('a)
      EmptyNode
      (TreeNode 'a (tree-node 'a) (tree-node 'a)))

(type coord (Coord double double))

(define (add-if-not-member a l)
  (if (member? a l) l (cons a l)))

(define (tree-operation f hash node)
  (if (null? hash)
      (f node)
      (if (head hash)
          (match node
             (EmptyNode (TreeNode nil (EmptyNode) 
                                  (tree-operation f (tail hash) (EmptyNode))))
             ((TreeNode coords left right) (TreeNode coords left
                                                     (tree-operation f (tail hash) right))))
          (match node
             (EmptyNode (TreeNode nil (tree-operation f (tail hash) (EmptyNode))
                                  (EmptyNode)))
             ((TreeNode coords left right) (TreeNode coords (tree-operation f (tail hash) left)
                                                     right))))))

(define (tree-navigate-to hash node)
  (if (null? hash) node
      (match node
             (EmptyNode node)
             ((TreeNode coords left right) (tree-navigate-to (tail hash) (if (head hash) right left))))))
        
(define (tree-contents node)
  (match node
         (EmptyNode nil)
         ((TreeNode coords left right) (append coords 
                                               (append (tree-contents left)
                                                       (tree-contents right))))))
(define (tree-at-least-one node)
  (match node
         (EmptyNode #f)
         ((TreeNode coords left right) (or (not (null? coords))
                                           (or (tree-at-least-one left)
                                               (tree-at-least-one right))))))

(define (tree-insert-op coord node)
  (match node
     (EmptyNode (TreeNode (cons coord nil) (EmptyNode) (EmptyNode)))
     ((TreeNode coords left right) (TreeNode (add-if-not-member coord coords) left right))))

(define (tree-delete-op coord node)
  (match node
     (EmptyNode (TreeNode (nil) (EmptyNode) (EmptyNode)))
     ((TreeNode coords left right) (TreeNode (remove coord coords) left right))))

(define (tree-delete-all-op coord node)
  (match node
     (EmptyNode node)
     ((TreeNode _ _ _) (EmptyNode))))

(type geohash-db (GeohashDB (tree-node (cons (coord))) int))

(define (make-geohashdb bits-of-precision)
  (GeohashDB (TreeNode nil (EmptyNode) (EmptyNode)) bits-of-precision))

(define (geohashdb-op db coord op)
  (let* (((GeohashDB root bits-of-precision) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (tree-operation op hash root)))

(define (geohashdb-insert db lat lon)
  (geohashdb-op db (Coord lat lon) (tree-insert-op (Coord lat lon))))
