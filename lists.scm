(define (map f l) (if (null? l) nil (cons (f (head l)) (map f (tail l)))))

(define (reverse-1 l revved)
  (if (null? l) revved
      (reverse-1 (tail l) (cons (head l) revved))))

(define (reverse l) (reverse-1 l nil))

(define (append-1 l appended)
  (if (null? l) appended
      (append-1 (tail l) (cons (head l) appended))))

(define (append a b)
  (append-1 (reverse a) b))

(define (member? a l)
  (if (null? l) #f
      (if (equals? a (head l)) #t
          (member? a (tail l)))))
