(define (map f l) (if (empty? l) nil (cons (f (head l)) (map f (tail l)))))
