(test test-nil
  (assert-equals nil (Nil) "nil should equal (Nil)"))

(test test-new-list
  (assert-equals (list 1 2 3) (Cons 1 (Cons 2 (Cons 3 (Nil)))) "list form should be equivalent to Cons list"))

(test test-all
  (assert-true (all id nil) "all id of nil is true")
  (assert-true (all id (list #t #t #t #t)) "all id list of true is true")
  (assert-false (all id (list #t #t #t #f #t)) "all id list of true+false is false")
  (assert-true (all (swap > 0) (range 1 10)) "all range 1 10 > 0 is true")
  (assert-false (all (swap > 1) (range 1 10))) "all range 1 10 > 1 is false")

(test test-append
  (assert-equals (append nil nil) nil "append nil nil is nil")
  (assert-equals (append (Nil) (Nil)) nil "append (Nil) (Nil) is nil")
  (assert-equals (append (Nil) (Nil)) (Nil) "append (Nil) (Nil) is (Nil)")
  (assert-equals (append nil nil) (Nil) "append nil nil is (Nil)")
  (assert-equals (append (list 1) (list 2 3)) (list 1 2 3) "append (1) to (2 3) is (1 2 3)")
  (assert-equals (append nil (list 1 2)) (list 1 2) "append nil to (1 2) is (1 2)")
  (assert-equals (append (list 1 2 3) nil) (list 1 2 3) "append (1 2 3) to nil is (1 2 3)"))

(test test-drop
  (assert-equals (drop 10 nil) nil "drop 10 nil should be nil")
  (assert-equals (drop 1 (list 1 2 3)) (list 2 3) "drop 1 from (1 2 3) is (2 3)")
  (assert-equals (drop 3 (list 1 2 3)) nil "drop 3 from (1 2 3) is nil")
  (assert-equals (drop 4 (list 1 2 3)) nil "drop 4 from (1 2 3) is nil"))

(define (always x) #t)
(define (never x) #f)

(test test-filter
  (assert-equals (filter (= 5) (list 4 5 4 5 4)) (list 5 5) "filter =5 (4 5 4 5 4) is (5 5)")
  (assert-equals (filter (= 5) nil) nil "filter =5 nil is nil")
  (assert-equals (filter (= 5) (list 1 2 3)) nil "filter =5 (1 2 3) is nil")
  (assert-equals (filter always (list 1 2 3)) (list 1 2 3) "filter always true (1 2 3) is (1 2 3)")
  (assert-equals (filter never (list 1 2 3)) nil "filter never true (1 2 3) is nil"))

(test test-fold
  (assert-equals (fold + 5 nil) 5 "fold + 5 nil = 5")
  (assert-equals (fold + 0 (list 1 2 3)) 6 "fold + 0 (1 2 3) = 9")
  (assert-equals (fold + 3 (list 1 2 3)) 9 "fold + 3 (1 2 3) = 9"))

(test test-length
  (assert-equals (length nil) 0 "length nil = 0")
  (assert-equals (length (list 1 2 3)) 3 "length (1 2 3) = 3")
  (assert-equals (length (range 1 1000)) 1000 "length (range 1 1000) = 1000"))

(test test-map
  (assert-equals (map (+ 5) nil) nil "map +5 nil = nil")
  (assert-equals (map id (list 1 2 3)) (list 1 2 3) "map id (1 2 3) = (1 2 3)")
  (assert-equals (map (+ 5) (list 1 2 3)) (list 6 7 8) "map (+ 5) (1 2 3) = (6 7 8)"))

(test test-member
  (assert-false (member? 5 nil) "5 is not a member of nil")
  (assert-false (member? 5 (list 1 2 3)) "5 is not a member of (1 2 3)")
  (assert-true (member? 5 (range 1 5)) "5 is a member of (range 1 5)"))

(test test-nth
  (assert-equals (nth 0 (list 1 2 3)) 1 "0th item in (1 2 3) is 1")
  (assert-equals (nth 1 (list 1 2 3)) 2 "1th item in (1 2 3) is 2")
  (assert-equals (nth 2 (list 1 2 3)) 3 "2th item in (1 2 3) is 3")
  (expect-exception FailException "should not be able to take nth 3 of (1 2 3)"
    (nth 3 (list 1 2 3)))
  (expect-exception FailException "should not be able to take nth 0 of nil"
    (nth 0 nil)))

(test test-remove
  (assert-equals (remove 0 nil) nil "remove 0 from nil is nil")
  (assert-equals (remove 0 (list 1 2 3)) (list 1 2 3) "remove 0 from (1 2 3) is (1 2 3)")
  (assert-equals (remove 1 (list 1 2 3)) (list 2 3) "remove 1 from (1 2 3) is (2 3)")
  (assert-equals (remove 2 (list 1 2 3)) (list 1 3) "remove 2 from (1 2 3) is (1 3)")
  (assert-equals (remove 3 (list 1 2 3)) (list 1 2) "remove 3 from (1 2 3) is (1 2)")
  (assert-equals (remove 3 (list 1 2 3 3)) (list 1 2 3) "remove 3 from (1 2 3 3) is (1 2 3)"))

(test test-replace-nth
  (assert-equals (replace-nth 0 5 (list 1 2 3)) (list 5 2 3) "replace 0th in (1 2 3) with 5 is (5 2 3)")
  (assert-equals (replace-nth 1 5 (list 1 2 3)) (list 1 5 3) "replace 1th in (1 2 3) with 5 is (1 5 3)")
  (assert-equals (replace-nth 2 5 (list 1 2 3)) (list 1 2 5) "replace 2th in (1 2 3) with 5 is (1 2 5)")
  (expect-exception FailException "should not be able to replace 3th item in (1 2 3)"
    (replace-nth 3 5 (list 1 2 3)))
  (expect-exception FailException "should not be able to replace 0th item in nil"
    (replace-nth 0 5 nil)))

(test test-reverse
  (assert-equals (reverse nil) nil "reverse of nil is nil")
  (assert-equals (reverse (list 1 2 3)) (list 3 2 1) "reverse of (1 2 3) is (3 2 1)")
  (assert-equals (reverse (reverse (range 1 5000))) (range 1 5000)
    "reverse of reverse of (1 .. 5000) is (1 .. 5000)"))

(test test-some
  (assert-false (some id nil) "some id of nil is false")
  (assert-false (some id (list #f #f #f #f)) "some id list of false is false")
  (assert-true (some id (list #f #f #f #t #f)) "some id list of false+true is true")
  (assert-true (some (swap > 5) (range 1 10)) "some range 1 10 > 5 is true")
  (assert-false (some (swap > 10) (range 1 10)) "some range 1 10 > 10 is false"))

(test test-take
  (assert-equals (take 5 nil) nil "take 5 of nil is nil")
  (assert-equals (take 5 (list 1 2 3)) (list 1 2 3) "take 5 of (1 2 3) is (1 2 3)")
  (assert-equals (take 3 (range 1 10)) (list 1 2 3) "take 3 of (1 .. 10) is (1 2 3)"))