(define (do-not-explode-1 n acc)
  (if (<= n 0) acc
      (do-not-explode-1 (- n 1) (+ 1 acc))))

(define (do-not-explode n) (do-not-explode-1 n 0))
