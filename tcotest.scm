(define (explode n) 
  (if (<= n 0) 0 
      (+ 1 (explode (- n 1)))))

(define (do-not-explode-1 n acc)
  (if (<= n 0) acc
      (do-not-explode-1 (- n 1) (+ 1 acc))))

(define (do-not-explode n) (do-not-explode-1 n 0))

(define (do-not-explode-let n)
  (letrec ((do-not-explode-1
             (lambda (n acc)
               (if (<= n 0) acc
                   (do-not-explode-1 (- n 1) (+ 1 acc))))))
    (do-not-explode-1 n 0)))

;; Dummy definition to allow even to refer to odd
(define (odd n) #f)
(define (even n) (if (= n 0) #t (odd (- n 1))))
(define (odd n) (if (= n 0) #f (even (- n 1))))
