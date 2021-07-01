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

(define (do-not-explode-assign n)
  (:= do-not-explode-1
      (lambda (n acc)
        (if (<= n 0) acc
            (do-not-explode-1 (- n 1) (+ 1 acc)))))
  (do-not-explode-1 n 0))

;; Dummy definition to allow even to refer to odd
(define (odd n) #f)
(define (even n) (if (= n 0) #t (odd (- n 1))))
(define (odd n) (if (= n 0) #f (even (- n 1))))

(test test-stackoverflow
  (expect-exception StackOverflowError "explode 10000 should cause stack overflow"
    (explode 10000)))

(test test-do-not-explode-define
  (expect-no-exception "do-not-explode 10000 should not cause stack overflow"
    (do-not-explode 10000)))

(test test-do-not-explode-let
  (expect-no-exception "do-not-explode-let 10000 should not cause stack overflow"
    (do-not-explode-let 10000)))

(test test-do-not-explode-assign
  (expect-no-exception "do-not-explode-assign 10000 should not cause stack overflow"
    (do-not-explode-assign 10000)))

(test test-odd-even
  (expect-no-exception "odd 10000 should not cause stack overflow"
    (odd 10000))
  (expect-no-exception "even 10000 should not cause stack overflow"
    (even 10000))
  (expect-no-exception "odd 10001 should not cause stack overflow"
    (odd 10001))
  (expect-no-exception "even 10001 should not cause stack overflow"
    (even 10001)))

