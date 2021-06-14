(define LATITUDE-MIN -90.0)
(define LATITUDE-MAX 90.0)
(define LONGITUDE-MIN -180.0)
(define LONGITUDE-MAX 180.0)

;;; Take the head of list l and prepend it to revved
;;; If l is empty, return revved, which should be the
;;; reversed list
(define (reverse-1 l revved)
  (if (empty? l) revved
      (reverse-1 (tail l) (cons (head l) revved))))

;;; Use a tail-recursive mechanism to reverse list
(define (reverse l) (reverse-1 l nil))


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
                                         (+ curr-bit 1) (cons #\0 bits))
       ; Otherwise the mid point is the new bottom and the geohash bit
       ; at this position is 1
           (geohash-1d-rec value-to-hash middle top bits-of-precision
                                         (+ curr-bit 1) (cons #\1 bits))))))

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
  (if (empty? l1) (reverse accum)

      ; If the second list is null, add the head of the first list onto the accum
      ; and then return the accumulated bits after reversing them
      (if (empty? l2) (reverse (cons (head l1) accum))

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

;;; Returns a geohash as a string, using geohash-2d to create a list of 0 and 1
;;; characters, and then just convert that list to a string using a built-in
;;; function.
(define (geohash lat lon bits-of-precision)
  (list->string (geohash-2d lat LATITUDE-MIN LATITUDE-MAX
                               lon LONGITUDE-MIN LONGITUDE-MAX
                               bits-of-precision)))
