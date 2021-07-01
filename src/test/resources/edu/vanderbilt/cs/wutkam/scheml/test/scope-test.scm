
(test test-function-scope
  ((lambda (x y z)
     (assert-equals x "foo" "param x should be foo")
     (assert-equals y 42 "param y should be 42")
     (assert-equals z (list 1 2 3) "param z should be (1 2 3)"))
     "foo" 42 (list 1 2 3)))

(test test-function-with-letscope
  ((lambda (x y z)
     (assert-equals x "foo" "param x should be foo")
     (assert-equals y 42 "param y should be 42")
     (assert-equals z (list 1 2 3) "param z should be (1 2 3)")
     (let ((x 47))
        (assert-equals x 47 "x in let should now be 47"))
     (assert-equals x "foo" "param x should again be foo")
     "foo" 42 (list 1 2 3))))

(test test-function-with-assign-scope
  ((lambda (x y z)
     (assert-equals x "foo" "param x should be foo")
     (assert-equals y 42 "param y should be 42")
     (assert-equals z (list 1 2 3) "param z should be (1 2 3)")
     (:= x 42)
     (assert-equals x 42 "param x should now be 42"))
     "foo" 42 (list 1 2 3)))

(test test-let-scope
  (let ((x "foo"))
     (assert-equals x "foo" "let binding x should be foo"))
  (expect-exception-on-unify UnifyException "x should not be visible outside of the let"
     x))

(test test-assign-scope
  (progn
    (:= x 42)
    (assert-equals x 42 "assign x should be 42")
    (progn
      (assert-equals x 42 "assign x should still be 42")
      (:= x "foo")
      (assert-equals x "foo" "assign x should now be foo"))
    (assert-equals x 42 "assign x should go back to 42")))



