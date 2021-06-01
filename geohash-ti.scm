(define LATITUDE-MIN -90.0)
(define LATITUDE-MAX 90.0)
(define LONGITUDE-MIN -180.0)
(define LONGITUDE-MAX 180.0)

(define (reverse-1 l revved)
  (if (null? l) revved
      (reverse-1 (tail l) (cons (head l) revved))))

(define (reverse l) (reverse-1 l nil))

(define (geohash-1d-rec value-to-hash bottom top bits-of-precision
                        curr-bit bits)
  (if (= curr-bit bits-of-precision) (reverse bits)
    (let ((middle (/. (+. bottom top) 2.0)))
       (if (< value-to-hash middle)
           (geohash-1d-rec value-to-hash bottom middle bits-of-precision
                                         (+ curr-bit 1) (cons #\0 bits))
           (geohash-1d-rec value-to-hash middle top bits-of-precision
                                         (+ curr-bit 1) (cons #\1 bits))))))

(define (geohash-1d value-to-hash bottom top bits-of-precision)
  (geohash-1d-rec value-to-hash bottom top bits-of-precision
                                0 nil))

(define (merge-bits l1 l2 accum)
  (if (null? l1) (reverse accum)
      (if (null? l2) (reverse (cons (head l1) accum))
          (merge-bits (tail l1) (tail l2) (cons (head l2) 
                                                (cons (head l1) accum))))))

(define (geohash-2d v1 v1-bottom v1-top v2 v2-bottom v2-top bits-of-precision)
  (let* ((extra-bit (mod bits-of-precision 2))
         (bop-half (/ bits-of-precision 2))
         (first-bits (geohash-1d v1 v1-bottom v1-top (+ bop-half extra-bit)))
         (second-bits (geohash-1d v2 v2-bottom v2-top bop-half)))
    (merge-bits first-bits second-bits nil)))

(define (geohash lat lon bits-of-precision)
  (list->string (geohash-2d lat LATITUDE-MIN LATITUDE-MAX
                               lon LONGITUDE-MIN LONGITUDE-MAX
                               bits-of-precision)))
