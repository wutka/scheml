(define LATITUDE-MIN -90.0)
(define LATITUDE-MAX 90.0)
(define LONGITUDE-MIN -180.0)
(define LONGITUDE-MAX 180.0)

;;; Use a tail-recursive mechanism to reverse a list
(define (reverse l) 
;;; Take the head of list l and prepend it to revved
;;; If l is empty, return revved, which should be the
;;; reversed list
  (letrec ((reverse-1
             (lambda (l revved)
               (if (null? l) revved
                   (reverse-1 (tail l) (cons (head l) revved))))))
    ;;; Call the tail-recursive func
    (reverse-1 l nil)))

;;; Returns a list created by applying function f to every element of list l
;;; This function is not tail recursive
(define (map f l) 
  (if (null? l) nil
      (cons (f (head l)) (map f (tail l)))))

;;; Appends list b to the end of list a
;;; To do this tail recursively, we reverse a, and then prepend
;;; each element of the reversed a onto b
(define (append a b)
  (letrec ((append-1
             (lambda (l appended)
               (if (null? l) appended
                   (append-1 (tail l) (cons (head l) appended))))))
    ;;; Call the tail-recursive func
    (append-1 (reverse a) b)))

;;; Returns #t if a is a member of list l
(define (member? a l)
  (if (null? l) #f
      (if (equals? a (head l)) #t
          (member? a (tail l)))))

;;; Removes the first occurrence of a from list l
;;; To do this recursively, we create an accumulator that
;;; contains the nodes we have already checked, in reverse order
;;; from the original list.
(define (remove a l) 
  (letrec ((remove-1
             (lambda (a l acc)
;;; If we get to the end of l without finding the item, acc will
;;; just be the reverse of l, so we return (reverse acc)
                (if (null? l) (reverse acc)
;;; Otherwise, at the point we find a in l, acc has everything in l
;;; before that point in reverse order, and the tail of l is everything
;;; that comes after it, so the list with a removed is
;;; found by appending the tail of l, which is in the correct order
;;; to the reverse of acc
                    (if (equals? a (head l)) (append (reverse acc) (tail l))
                        (remove-1 a (tail l) (cons (head l) acc)))))))
  (remove-1 a l nil)))

;;; Takes n items from the front of a list
;;; To do this tail-recursively, we repeatedly take the head
;;; of l and cons it onto acc. When we either have done this n
;;; times or gotten to the end of l, acc now has the items in
;;; the reverse order that we want them, so we reverse it
(define (take n l)
  (letrec ((take-1 (lambda (n l acc)
                     (if (null? l) (reverse acc)
                         (if (<= n 0) (reverse acc)
                             (take-1 (- n 1) (tail l)
                                     (cons (head l) acc)))))))
    (take-1 n l nil)))

;;; Returns the length of a list
;;; To do this tail-recursively we keep a count n and
;;; when we get to the end of l, n is the number of nodes
(define (length l) 
  (letrec ((length-1
             (lambda (l n)
                (if (null? l) n
                    (length-1 (tail l) (+ n 1))))))
    (length-1 l 0)))

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

;;; Define a tree-node type that can contain any type of value
;;; EmptyNode represents the absence of a part of the tree
;;; while TreeNode represents a live node
(type tree-node ('a)
      EmptyNode
      (TreeNode 'a (tree-node 'a) (tree-node 'a)))

;;; Create data type to hold lat and lon coordinates as a single value
;;; so they can be stored in a list
(type coord (Coord double double))

;;; Adds a to list l if a is not a member of l already
(define (add-if-not-member a l)
  (if (member? a l) l (cons a l)))

;;; Traverses a tree according to a geohash, creating new nodes when
;;; necessary, and then returns an updated tree that results from
;;; having applied function f to the node that was reached by the
;;; geohash (function f must return a tree-node)
(define (tree-update f hash node)
  (if (null? hash)
      ;;; If we reach the end of the hash, apply f to this node
      (f node)

      (if (head hash)
          ;;; If the next hash is true, descend down the right side
          ;;; If the current node is empty, create a new node
          (match node
             (EmptyNode (TreeNode nil (EmptyNode) 
                             (tree-update f (tail hash) (EmptyNode))))
             ((TreeNode coords left right) 
                   (TreeNode coords left
                       (tree-update f (tail hash) right))))
          ;;; Otherwise, the next has is false, descend fown the right
          ;;; side, and if the current node is empty, create a new node
          (match node
             (EmptyNode (TreeNode nil 
                             (tree-update f (tail hash) (EmptyNode))
                             (EmptyNode)))
             ((TreeNode coords left right) 
                  (TreeNode coords 
                       (tree-update f (tail hash) left)
                                                     right))))))

;;; Uses a geohash to traverse to a specific node in a tree and runs f
;;; on that node. If at any time it encounters an empty node,
;;; it stops its descent and returns f of the empty node
;;; Since this function doesn't mutate the tree, it doesn't create
;;; nodes along the way
(define (tree-query f hash node)
  (if (null? hash) (f node)
      (match node
             (EmptyNode (f node))
             ((TreeNode coords left right) 
              (tree-query f (tail hash) 
                                (if (head hash) right left))))))
        
;;; Returns the contents of a node and all its children
(define (tree-contents node)
  (match node
         ;;; Empty node means there isn't anything where we are looking
         (EmptyNode nil)
         ;;; Return this node's coords, plus the contents of its left and
         ;;; right nodes all appended together
         ((TreeNode coords left right) 
               (append coords (append (tree-contents left)
                                      (tree-contents right))))))

;;; Returns true if the tree node contains the specified coord
(define (tree-contains coord node)
  (match node
         (EmptyNode #f)
         ((TreeNode coords _ _) (member? coord coords))))

;;; Returns true if this node, or any of its children contain at least
;;; one coord
(define (tree-at-least-one node)
  (match node
         ;;; If the node is empty, that's a no
         (EmptyNode #f)
         ;;; Otherwise, if coords is not null, or the left has at least one
         ;;; or the right has at least one, then this node has at least one
         ((TreeNode coords left right) 
               (or (not (null? coords))
                   (or (tree-at-least-one left)
                       (tree-at-least-one right))))))

;;; Inserts a coordiate at a partidular node in the tree
(define (tree-insert-op coord node)
  (match node
    ;;; if the node is empty, create a new node with a list containing
    ;;; only the coord
     (EmptyNode (TreeNode (cons coord nil) (EmptyNode) (EmptyNode)))
     ;;; Otherwise add coord to coords if it isn't in there already,
     ;;; and return a new tree node created from the old one
     ((TreeNode coords left right) 
           (TreeNode (add-if-not-member coord coords) left right))))

;;; Deletes a coordinate at a particular node in the tree
(define (tree-delete-op coord node)
  (match node
     ;;; If the node is already empty, the delete has no effect
     (EmptyNode (EmptyNode))
     ;;; Otherwise, create a new tree node from the old one,
     ;;; removing coord from the coords
     ((TreeNode coords left right) 
           (TreeNode (remove coord coords) left right))))

;;; Deletes a node and all its children
;;; We can just return an empty node here, since either way
;;; that's what we want this node to be
(define (tree-delete-all-op node)
  (EmptyNode))

;;; Define a GeoDB type that contains a tree with a list of coordinates
;;; and also an int that holds the default bits-of-precision
(type geodb (GeoDB (tree-node (cons (coord))) int))

;;; Create a pair for returning multiple results. Several options mutate
;;; the tree but also have return values
(type pair ('a 'b) (Pair 'a 'b))

;;; Creates a new GeoDB with the specified bits of precision
(define (make-geodb bits-of-precision)
  (GeoDB (TreeNode nil (EmptyNode) (EmptyNode)) bits-of-precision))

;;; Execute tree update op at the location in the tree
;;; determined by the geohash of coord, and return the updated GeoDB
(define (geodb-op db coord op)
  (let* (((GeoDB root bits-of-precision) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (GeoDB (tree-update op hash root) bits-of-precision)))

;;; Execute a tree query op at the location in the tree determined
;;; by the geohash of coord, and return the results of the query func
(define (geodb-at db coord op)
  (let* (((GeoDB root bits-of-precision) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (tree-query op hash root)))

;;; Execute tree operation op at the location in the tree
;;; determined by the geohash of coord only up to bits-of-precision
;;; bits, and return the updated GeoDB
(define (geodb-op-bits db coord op bits-of-precision)
  ;;; Need to hold on to the original GeoDB bits-of-precision
  ;;; because we need that when returning the updated GeoDB
  (let* (((GeoDB root orig-bop) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (GeoDB (tree-update op hash root) orig-bop)))

;;; Executes tree query op at the location in the tree determined
;;; by the geohash of coord only up to bits-of-precision
(define (geodb-at-bits db coord op bits-of-precision)
  ;;; Unlike the update, we don't need to save the old bits of
  ;;; precision because this operation doesn't mutate the tree
  (let* (((GeoDB root _) db)
         ((Coord lat lon) coord)
         (hash (geohash lat lon bits-of-precision)))
    (tree-query op hash root)))

;;; Inserts a new coordinate into the tree returning a pair containing
;;; a boolean (always false) and the new GeoDB
(define (geodb-insert lat lon db)
  (let ((coord (Coord lat lon)))
    ;;; k
    (Pair #f (geodb-op db coord (tree-insert-op coord)))))

;;; Deletes a coordinate from the tree, returning a part containing
;;; a boolean indicator of whether or not the coord was in the db
;;; or not, and the new GeoDB
(define (geodb-delete lat lon db)
  (let* ((coord (Coord lat lon))
         (retval (geodb-at db coord (tree-contains coord))))
    (Pair retval (geodb-op db coord (tree-delete-op coord)))))
    
;;; Deletes all the locations at or below the specified coordinate
;;; at the specified bits of precision, and returns a pair containing
;;; a list of all the items deleted and the new GeoDB
(define (geodb-delete-all lat lon bits-of-precision db)
  (let* ((coord (Coord lat lon))
         (retval (geodb-at-bits db coord tree-contents bits-of-precision)))
    (Pair retval 
          (geodb-op-bits db coord tree-delete-all-op bits-of-precision))))

;;; Returns true if the GeoDB contains at least one item at the
;;; specified coordinate and bits-of-precision
(define (geodb-contains lat lon bits-of-precision db)
  (let* ((coord (Coord lat lon)))
    (geodb-at-bits db coord tree-at-least-one bits-of-precision)))

;;; Returns a list of all the coordinates at the same level or below the
;;; specified coordinate at the specified bits of precision
(define (geodb-nearby lat lon bits-of-precision db)
  (let* ((coord (Coord lat lon)))
    (geodb-at-bits db coord tree-contents bits-of-precision)))
          
;;; Allows you to chain together a series of GeoDB updates (as long
;;; as the Pair returned by the update returns the same type, so
;;; insert and delete can be chained, but not with other operations)
;;; Since the language doesn't let you mutate values, this keeps you
;;; from making deep chains of let expressions to hold each intermediate
;;; version of the GeoDB
(define (geodb-chain db ops)
  ;;; If we hit the end of the list, return the last version of the GeoDB
  (if (null? ops) db
      ;;; Execute the first function in the ops list, and bind the
      ;;; resulting database value to db-next, while ignoring the first
      ;;; part of the Pair
      (let (((Pair _ db-next) ((head ops) db)))
        ;;; process the next item in the chain with the updated db
        (geodb-chain db-next (tail ops)))))
