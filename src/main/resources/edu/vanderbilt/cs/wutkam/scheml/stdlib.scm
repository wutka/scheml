;;; Standard library functions written in Scheml

(define (swap f a b) (f b a))

(define (just? o)
    (match o
      (Nothing #f)
      ((Just _) #t)))

(define (nothing? o)
  (match o
    (Nothing #t)
    ((Just _) #f)))

(define (just o)
  (match o
    (Nothing (fail "Tried to get Just on Nothing"))
    ((Just x) x)))
