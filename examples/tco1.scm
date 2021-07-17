(define (explode n) 
  (if (<= n 0) 0 
      (+ 1 (explode (- n 1)))))
