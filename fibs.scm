(define (badfib n) 
  (if (< n 3) 1 
      (+ (badfib (- n 1)) (badfib (- n 2)))))

(define (goodfib-1 n v1 v2)
  (if (<= n 1) v2
      (goodfib-1 (- n 1) v2 (+ v2 v1))))

(define (goodfib n) (goodfib-1 n 0 1))
