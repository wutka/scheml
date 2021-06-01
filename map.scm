(define (map f l) (if (null? l) nil (cons (f (head l)) (map f (tail l)))))
