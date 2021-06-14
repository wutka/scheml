(define (fact n)
  (letrec ((fact-1 (lambda (n acc)
                     (if (< n 2) acc
                         (fact-1 (- n 1) (* acc n))))))
    (fact-1 n 1)))
