(test test-int-match
   (match 5
      (5 "foo")
      (6 "bar")
      (7 "baz"))
   (assert-warning "not exhaustive" "int match with no wildcard should not be exhaustive")
   (assert-no-warning "is redundant" "int match with unique items should not be redundant")
   )

(test test-int-wildcard-match
   (match 5
      (5 "foo")
      (6 "bar")
      (7 "baz")
      (_ "quux"))
   (assert-no-warning "not exhaustive" "int match with wildcard should be exhaustive")
   (assert-no-warning "is redundant" "int match with unique items should not be redundant"))

(test test-int-redundant-match
   (match 5
      (5 "foo")
      (5 "bar")
      (7 "baz"))
   (assert-warning "not exhaustive" "int match with no wildcard should not be exhaustive")
   (assert-warning "is redundant" "int match with unique items should not be redundant")
   )

(test test-int-wildcard-redundant-match
   (match 5
      (5 "foo")
      (5 "bar")
      (7 "baz")
      (_ "quux"))
   (assert-no-warning "not exhaustive" "int match with wildcard should be exhaustive")
   (assert-warning "is redundant" "int match with unique items should not be redundant"))

(type bool3 (Bool3 bool bool bool))

;; The following match expression is due to Gerard Berry (according to Simon Peyton Jones)
(test test-berry-pattern
  (match (Bool3 #t #f #t)
     ((Bool3 #t #f _) (print "1\n"))
      ((Bool3 #f _ #t) (print "3\n"))
      ((Bool3 _ #t #f) (print "2\n")))
   (assert-warning "not exhaustive" "berry match is not exhaustive")
   (assert-no-warning "is redundant" "berry match is not redundant"))



