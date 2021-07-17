(define (do-not-explode-let n)
  (letrec ((do-not-explode-1
             (lambda (n acc)
               (if (<= n 0) acc
                   (do-not-explode-1 (- n 1) (+ 1 acc))))))
    (do-not-explode-1 n 0)))
