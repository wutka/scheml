;;; Use a tail-recursive mechanism to reverse a list
(define (reverse l) 
;;; Take the head of list l and prepend it to revved
;;; If l is empty, return revved, which should be the
;;; reversed list
  (letrec ((reverse-1
             (lambda (l revved)
               (if (null? l) revved
                   (reverse-1 (tail l) (cons (head l) revved))))))
    ;;; Call the tail-recursive func
    (reverse-1 l nil)))

;;; Returns a list created by applying function f to every element of list l
;;; This function is not tail recursive
(define (map f l) 
  (if (null? l) nil
      (cons (f (head l)) (map f (tail l)))))

;;; Appends list b to the end of list a
;;; To do this tail recursively, we reverse a, and then prepend
;;; each element of the reversed a onto b
(define (append a b)
  (letrec ((append-1
             (lambda (l appended)
               (if (null? l) appended
                   (append-1 (tail l) (cons (head l) appended))))))
    ;;; Call the tail-recursive func
    (append-1 (reverse a) b)))

;;; Returns #t if a is a member of list l
(define (member? a l)
  (if (null? l) #f
      (if (equals? a (head l)) #t
          (member? a (tail l)))))

;;; Removes the first occurrence of a from list l
;;; To do this recursively, we create an accumulator that
;;; contains the nodes we have already checked, in reverse order
;;; from the original list.
(define (remove a l) 
  (letrec ((remove-1
             (lambda (a l acc)
;;; If we get to the end of l without finding the item, acc will
;;; just be the reverse of l, so we return (reverse acc)
                (if (null? l) (reverse acc)
;;; Otherwise, at the point we find a in l, acc has everything in l
;;; before that point in reverse order, and the tail of l is everything
;;; that comes after it, so the list with a removed is
;;; found by appending the tail of l, which is in the correct order
;;; to the reverse of acc
                    (if (equals? a (head l)) (append (reverse acc) (tail l))
                        (remove-1 a (tail l) (cons (head l) acc)))))))
  (remove-1 a l nil)))

;;; Takes n items from the front of a list
;;; To do this tail-recursively, we repeatedly take the head
;;; of l and cons it onto acc. When we either have done this n
;;; times or gotten to the end of l, acc now has the items in
;;; the reverse order that we want them, so we reverse it
(define (take n l)
  (letrec ((take-1 (lambda (n l acc)
                     (if (null? l) (reverse acc)
                         (if (<= n 0) (reverse acc)
                             (take-1 (- n 1) (tail l)
                                     (cons (head l) acc)))))))
    (take-1 n l nil)))

;;; Drops n items from the front of a list
(define (drop n l)
  (if (null? l) nil
      (if (<= n 0) l (drop (- n 1) (tail l)))))

;;; Returns the length of a list
;;; To do this tail-recursively we keep a count n and
;;; when we get to the end of l, n is the number of nodes
(define (length l) 
  (letrec ((length-1
             (lambda (l n)
                (if (null? l) n
                    (length-1 (tail l) (+ n 1))))))
    (length-1 l 0)))

(define (nth n l)
  (if (null? l) (fail "Ran out of elements looking for nth element")
      (if (= n 0) (head l)
          (nth (- n 1) (tail l)))))

;;; Fold takes a function of type 'a -> 'b -> 'b and
;;; applies it to a list of 'a, where each element in the list
;;; is the first argument to f, and the second argument is the
;;; previous value of (f a b). For example, to sum the elements
;;; a list: (fold + 0 (list 1 2 3 4 5 6))
(define (fold f b l)
  (if (null? l) b
      (fold f (f (head l) b) (tail l))))

;;; Returns true if all elements of a list match predicate p
(define (all p l)
  (if (null? l) #t
      (if (not (p (head l))) #f
          (all p (tail l)))))

;;; Returns true if at least one element of a list matches predicate p
(define (some p l)
  (if (null? l) #f
      (if (p (head l)) #t
          (some p (tail l)))))
