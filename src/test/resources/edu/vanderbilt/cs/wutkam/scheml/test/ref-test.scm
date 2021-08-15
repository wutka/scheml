(test ref-test
  (let ((fooref (Ref 5)))
    (assert-equals (! fooref) 5 "deref (Ref 5) should = 5")
    (<- fooref 999)
    (assert-equals (! fooref) 999 "changed ref to 999 should = 999")))