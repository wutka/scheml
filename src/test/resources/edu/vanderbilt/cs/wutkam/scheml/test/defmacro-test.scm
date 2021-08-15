
(define (ends-with-void body)
  (match body
    ((SexprList lst) (equals? (head (reverse lst)) `void))
    (_ #f)))

(defmacro my-when (test &rest body)
    (let ((end-void (if (ends-with-void body) `() `(void))))
      `(if ,test (progn ,@body ,@end-void) void)))

(test test-my-when
  (assert-equals (my-when (> 6 5) (printf "foo\n")) void)
  (expect-exception FailException "my-when should have done a fail"
    (my-when (> 6 5) (fail "foo")))
  (assert-equals (my-when (> 5 6) (fail "foo")) void "my-when should not invoke fail"))
