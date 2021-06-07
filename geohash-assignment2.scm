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

(define (length-1 l n)
  (if (null? l) n
      (length-1 (tail l) (+ n 1))))

(define (length l) (length-1 l 0))

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

(define (tree-update-at f hash node)
  (if (null? hash)
      (f node)
      (if (head hash)
          (match node
             (EmptyNode (TreeNode nil (EmptyNode) 
                                  (tree-update-at f (tail hash) (EmptyNode))))
             ((TreeNode coords left right) (TreeNode coords left
                                                     (tree-update-at f (tail hash) right))))
          (match node
             (EmptyNode (TreeNode nil (tree-update-at f (tail hash) (EmptyNode))
                                  (EmptyNode)))
             ((TreeNode coords left right) (TreeNode coords (tree-update-at f (tail hash) left)
                                                     right))))))

(define (tree-navigate-to f hash node)
  (if (null? hash) (f node)
      (match node
             (EmptyNode (f node))
             ((TreeNode coords left right) (tree-navigate-to f (tail hash) (if (head hash) right left))))))
        
(define (tree-contents node)
  (match node
         (EmptyNode nil)
         ((TreeNode coords left right) (append coords 
                                               (append (tree-contents left)
                                                       (tree-contents right))))))

(define (tree-contains coord node)
  (match node
         (EmptyNode #f)
         ((TreeNode coords _ _) (member? coord coords))))

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

(define (tree-delete-all-op node)
  (match node
     (EmptyNode node)
     ((TreeNode _ _ _) (EmptyNode))))

(type geodb (GeoDB (tree-node (cons (coord))) int))

(type pair ('a 'b) (Pair 'a 'b))

(define (make-geodb bits-of-precision)
  (GeoDB (TreeNode nil (EmptyNode) (EmptyNode)) bits-of-precision))

(define (geodb-op db coord op)
  (let* (((GeoDB root bits-of-precision) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (GeoDB (tree-update-at op hash root) bits-of-precision)))

(define (geodb-at db coord op)
  (let* (((GeoDB root bits-of-precision) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (tree-navigate-to op hash root)))

(define (geodb-op-bits db coord op bits-of-precision)
  (let* (((GeoDB root orig-bop) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (GeoDB (tree-update-at op hash root) orig-bop)))

(define (geodb-at-bits db coord op bits-of-precision)
  (let* (((GeoDB root _) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (tree-navigate-to op hash root)))

(define (geodb-insert lat lon db)
  (let ((coord (Coord lat lon)))
    (Pair #f (geodb-op db coord (tree-insert-op coord)))))

(define (geodb-delete lat lon db)
  (let* ((coord (Coord lat lon))
         (retval (geodb-at db coord (tree-contains coord))))
    (Pair retval (geodb-op db coord (tree-delete-op coord)))))
    
 (define (geodb-delete-all lat lon bits-of-precision db)
   (let* ((coord (Coord lat lon))
          (retval (geodb-at-bits db coord tree-contents bits-of-precision)))
     (Pair retval (geodb-op-bits db coord tree-delete-all-op bits-of-precision))))

 (define (geodb-contains lat lon bits-of-precision db)
   (let* ((coord (Coord lat lon)))
     (geodb-at-bits db coord tree-at-least-one bits-of-precision)))

 (define (geodb-nearby lat lon bits-of-precision db)
   (let* ((coord (Coord lat lon)))
     (geodb-at-bits db coord tree-contents bits-of-precision)))
          
(define (geodb-chain db ops)
  (if (null? ops) db
      (let (((Pair _ db-next) ((head ops) db)))
        (geodb-chain db-next (tail ops)))))
