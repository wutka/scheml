(define (is-void-type type-expr)
  (match type-expr
     ((FunctionType _ f-ret) (is-void-type f-ret))
     (VoidType #t)
     (_ #f)))

(define (ends-with-void body)
  (match body
    ((SexprList lst) (is-void-type (eval-type (head (reverse lst)))))
    (_ #f)))

(defmacro my-when (test &rest body)
    (let ((end-void (if (ends-with-void body) `() `(void))))
      `(if ,test (progn ,@body ,@end-void) void)))
