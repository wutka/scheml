(test test-simple-type-inference
  (assert-equals (type-of 5) (IntType) "5 has type int")
  (assert-equals (type-of 5.4) (DoubleType) "5.4 has type double")
  (assert-equals (type-of #t) (BoolType) "#t has type bool")
  (assert-equals (type-of #f) (BoolType) "#f has type bool")
  (assert-equals (type-of #\f) (CharType) "#\f has type char")
  (assert-equals (type-of "foo") (StringType) "foo has type string"))

(test test-function-type-inference
  (assert-equals (type-of (lambda (x) x)) (FunctionType (AnyType "'a") (AnyType "'a")) "identity lambda has type 'a -> 'a")
  (assert-equals (type-of (lambda (x) (+ x 5))) (FunctionType (IntType) (IntType)) "lambda x+5 has type int -> int")
  (assert-equals (type-of map) (FunctionType (FunctionType (AnyType "'a") (AnyType "'b"))
                                             (FunctionType (AbstractType "cons" (list (AnyType "'a")))
                                                           (AbstractType "cons" (list (AnyType "'b")))))
                 "map has type ('a -> 'b) -> cons 'a -> cons 'b")
  (assert-equals (type-of (lambda (x y) (printf "%d %f\n" x y))) 
      (FunctionType (IntType) (FunctionType (DoubleType) (VoidType)))
      "lambda x y print %d %f x y has type int -> double -> void"))

(test test-list-type-inference
  (assert-equals (type-of nil) (AbstractType "cons" (list (AnyType "'a"))) "nil has type cons 'a")
  (assert-equals (type-of (list 1 2 3)) (AbstractType "cons" (list (IntType))) "(1 2 3) has type cons 'a")
  (assert-equals (type-of (list id id id)) (AbstractType "cons" (list (FunctionType (AnyType "'a") (AnyType "'a"))))
                  "(id id id) has type cons ('a -> 'a)"))

(type pair ('a 'b) (Pair 'a 'b))
(test test-abstract-type-inference
  (assert-equals (type-of Pair) (FunctionType (AnyType "'a")
                                              (FunctionType (AnyType "'b")
                                                            (AbstractType "pair" (list (AnyType "'a") (AnyType "'b")))))
                  "Pair is type pair 'a 'b")
  (assert-equals (type-of (Pair 5)) (FunctionType (AnyType "'a")
                                                  (AbstractType "pair" (list (IntType) (AnyType "'a"))))
                  "Pair 5 is type pair int 'a")
  (assert-equals (type-of (Pair 5 "foo")) (AbstractType "pair" (list (IntType) (StringType)))
                 "Pair 5 foo has type pair int string"))

(test test-if
  (assert-equals (type-of (if #t "foo" "bar")) (StringType) "if #t foo bar has type string")
  (assert-equals (type-of (if #t 42 37)) (IntType) "if #t 42 37 has type int")
  (assert-equals (type-of (if #f (print "foo") (print "bar"))) (VoidType) "if #f print .. print .. has type void"))

(test test-partial-application-type-inference
  (assert-equals (type-of (+ 5)) (FunctionType (IntType) (IntType)) "+ 5 should have type int -> int")
  (assert-equals (type-of (map (+ 5))) (FunctionType (AbstractType "cons" (list (IntType)))
                                                     (AbstractType "cons" (list (IntType))))
                 "(map (+ 5)) should have type cons int -> cons int"))
