(define (matchlist l)
  (match l
         ((1 2 3 4) 1)
         ((1 2 2 5) 2)
         ((1 2 3 4) 5)
         (_ 6)))
