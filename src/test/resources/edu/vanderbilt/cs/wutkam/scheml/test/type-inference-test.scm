(test test-simple-type-inference
  (assert-equals (type-of 5) "int" "5 has type int")
  (assert-equals (type-of 5.4) "double" "5.4 has type double")
  (assert-equals (type-of #t) "bool" "#t has type bool")
  (assert-equals (type-of #f) "bool" "#f has type bool")
  (assert-equals (type-of #\f) "char" "#\f has type char")
  (assert-equals (type-of "foo") "string" "foo has type string"))

(test test-function-type-inference
  (assert-equals (type-of (lambda (x) x)) "'a -> 'a" "identity lambda has type 'a -> 'a")
  (assert-equals (type-of (lambda (x) (+ x 5))) "int -> int" "lambda x+5 has type int -> int")
  (assert-equals (type-of map) "('a -> 'b) -> cons 'a -> cons 'b" "map has type ('a -> 'b) -> cons 'a -> cons 'b")
  (assert-equals (type-of (lambda (x y) (printf "%d %f\n" x y))) "int -> double -> void"
      "lambda x y print %d %f x y has type int -> double -> void"))

(test test-list-type-inference
  (assert-equals (type-of nil) "cons 'a" "nil has type cons 'a")
  (assert-equals (type-of (list 1 2 3)) "cons int" "(1 2 3) has type cons 'a")
  (assert-equals (type-of (list id id id)) "cons ('a -> 'a)" "(id id id) has type cons ('a -> 'a)"))

(type pair ('a 'b) (Pair 'a 'b))
(test test-abstract-type-inference
  (assert-equals (type-of Pair) "'a -> 'b -> pair 'a 'b" "Pair is type pair 'a 'b")
  (assert-equals (type-of (Pair 5)) "'a -> pair int 'a" "Pair 5 is type pair int 'a")
  (assert-equals (type-of (Pair 5 "foo")) "pair int string" "Pair 5 foo has type pair int string"))

(test test-if
  (assert-equals (type-of (if #t "foo" "bar")) "string" "if #t foo bar has type string")
  (assert-equals (type-of (if #t 42 37)) "int" "if #t 42 37 has type int")
  (assert-equals (type-of (if #f (print "foo") (print "bar"))) "void" "if #f print .. print .. has type void"))

(test test-partial-application-type-inference
  (assert-equals (type-of (+ 5)) "int -> int" "+ 5 should have type int -> int")
  (assert-equals (type-of (map (+ 5))) "cons int -> cons int" "(map (+ 5)) should have type cons int -> cons int"))

