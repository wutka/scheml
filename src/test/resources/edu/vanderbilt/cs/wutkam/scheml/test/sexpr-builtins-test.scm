(test test-sexpr-tests
   (assert-true (sexpr-bool? (SexprBool #t)))
   (assert-false (sexpr-bool? (SexprInt 5)))
   (assert-true (sexpr-char? (SexprChar #\A)))
   (assert-false (sexpr-char? (SexprInt 5)))
   (assert-true (sexpr-double? (SexprDouble 3.14)))
   (assert-false (sexpr-double? (SexprInt 5)))
   (assert-true (sexpr-int? (SexprInt 5)))
   (assert-false (sexpr-int? (SexprString "foo")))
   (assert-true (sexpr-list? (SexprList (Nil))))
   (assert-false (sexpr-list? (SexprString "foo")))
   (assert-true (sexpr-string? (SexprString "foo")))
   (assert-false (sexpr-string? (SexprInt 5)))
   (assert-true (sexpr-symbol? `foo))
   (assert-false (sexpr-symbol? (SexprInt 5))))

(test test-sexpr-conversions
   (assert-equals (sexpr->bool (SexprBool #t)) #t "Sexpr Bool conversion should return same bool")
   (expect-exception FailException "Should not be able to convert non-SexprBool to bool"
     (sexpr->bool (SexprInt 5)))
   (assert-equals (sexpr->char (SexprChar #\A)) #\A "Sexpr Char conversion should return same char")
   (expect-exception FailException "Should not be able to convert non-SexprChar to char"
     (sexpr->char (SexprInt 5)))
   (assert-equals (sexpr->double (SexprDouble 3.14)) 3.14 "Sexpr Double conversion should return same double")
   (expect-exception FailException "Should not be able to convert non-SexprDouble to double"
     (sexpr->double (SexprInt 5)))
   (assert-equals (sexpr->int (SexprInt 5)) 5 "Sexpr Int conversion should return same int")
   (expect-exception FailException "Should not be able to convert non-SexprInt to int"
     (sexpr->int (SexprString "foo")))
   (assert-equals (sexpr->list (SexprList nil)) nil "Sexpr List conversion should return same list")
   (assert-equals (sexpr->list `(1 2 3)) (list (SexprInt 1) (SexprInt 2) (SexprInt 3))  "Sexpr List conversion should return same list")
   (expect-exception FailException "Should not be able to convert non-SexprList to list"
     (sexpr->list (SexprInt 5)))
   (assert-equals (sexpr->string (SexprString "foo")) "foo" "Sexpr Int conversion should return same int")
   (expect-exception FailException "Should not be able to convert non-SexprString to string"
     (sexpr->string (SexprInt 5)))
   (assert-equals (->string (sexpr->symbol `foo)) "foo" "Sexpr Symbol conversion should return same symbol"))
   (expect-exception FailException "Should not be able to convert non-SexprSymbol to symbol"
     (sexpr->symbol (SexprInt 5)))

(test test-sexpr-unified-list-conversion
  (assert-true (list-convertible? `(1 2 3)) "`(1 2 3) should be convertible to a list of int")
  (assert-false (list-convertible? `(1 foo "bar")) "`(1 foo \"bar\") should not be convertible to a list")

  (assert-equals (convert-sexpr-list `(1 2 3)) (list 1 2 3) "convert-sexpr-list `(1 2 3) should be (1 2 3)")
  (expect-exception FailException "Converting non-convertible list should fail"
    (convert-sexpr-list `(1 foo "bar"))))

(test test-eval
  (assert-equals (eval (SexprBool #f)) (SexprBool #f) "Eval (SexprBool #f) should be (SexprBool #f)")
  (assert-equals (eval `(+ 3 4)) (SexprInt 7) "Eval `(+ 3 4) should be (SexprInt 7)")
  (assert-equals
      (let ((foobar "f00b@r"))
         (eval `foobar)) (SexprString "f00b@r") "Eval of symbol in environment should yield symbol value")
  )
