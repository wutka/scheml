(define (ends-with-void body)
  (match body
    ((SexprList lst) (equals? (head (reverse lst)) `void))
    (_ #f)))

(defmacro my-when (test &rest body)
    (let ((end-void (if (ends-with-void body) `() `(void))))
      `(if ,test (progn ,@body ,@end-void) void)))
