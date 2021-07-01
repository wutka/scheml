(test test-add
  (assert-equals (+ 0 0) 0 "0 + 0 = 0")
  (assert-equals (+ -5 4) -1 "-5 + 4 = -1"))

(test test-subtract
  (assert-equals (- 0 0) 0 "0 - 0 = 0")
  (assert-equals (- 5 5) 0 "5 - 5 = 0")
  (assert-equals (- 5 7) -2 "5 - 7 = -2"))

(test test-mult
  (assert-equals (* 0 0) 0 "0 * 0 = 0")
  (assert-equals (* 7 1) 7 "7 * 1 = 7")
  (assert-equals (* 7 -7) -49 "7 * -7 = -49"))

(test test-div
  (assert-equals (/ 0 1) 0 "0 / 1 = 0")
  (assert-equals (/ 8 2) 4 "8 / 2 = 4")
  (assert-equals (/ 9 2) 4 "9 / 2 = 4")
  (expect-exception ArithmeticException "Can't divide by 0"
    (/ 5 0)))

(test test-div2
  (assert-equals (div 0 1) 0 "0 div 1 = 0")
  (assert-equals (div 8 2) 4 "8 div 2 = 4")
  (assert-equals (div 9 2) 4 "9 div 2 = 4")
  (expect-exception ArithmeticException "Can't divide by 0"
    (div 5 0)))

(test test-mod
  (assert-equals (% 5 1) 0 "anything mod 1 = 0")
  (assert-equals (% 5 2) 1 "5 % 2 = 1")
  (assert-equals (% 5 3) 2 "5 % 3 = 2")
  (assert-equals (% -5 3) -2 "-5 % 3 = -2")
  (assert-equals (% 5 -3) 2 "5 % -3 = 2")
  (assert-equals (% -5 -3) -2 "-5 % -3 = -2")
  (expect-exception ArithmeticException "Can't divide/modulo by 0"
    (% 5 0)))

(test test-mod2
  (assert-equals (mod 5 1) 0 "anything mod 1 = 0")
  (assert-equals (mod 5 2) 1 "5 mod 2 = 1")
  (assert-equals (mod 5 3) 2 "5 mod 3 = 2")
  (assert-equals (mod -5 3) -2 "-5 mod 3 = -2")
  (assert-equals (mod 5 -3) 2 "5 mod -3 = 2")
  (assert-equals (mod -5 -3) -2 "-5 mod -3 = -2")
  (expect-exception ArithmeticException "Can't divide/modulo by 0"
    (mod 5 0)))

(test test-min
  (assert-equals (min 1 2) 1 "min 1 2 = 1")
  (assert-equals (min 2 1) 1 "min 2 1 = 1")
  (assert-equals (min 1 1) 1 "min 2 1 = 1"))

(test test-max
  (assert-equals (max 1 2) 2 "max 1 2 = 2")
  (assert-equals (max 2 1) 2 "max 2 1 = 2")
  (assert-equals (max 1 1) 1 "max 2 1 = 1"))

(test test-add-double
  (assert-equals (+. 0.0 0.0) 0.0 "0.0 +. 0.0 = 0.0")
  (assert-equals (+. -5.0 4.0) -1.0 "-5.0 +. 4.0 = -1.0"))

(test test-subtract-double
  (assert-equals (-. 0.0 0.0) 0.0 "0.0 -. 0.0 = 0.0")
  (assert-equals (-. 5.0 5.0) 0.0 "5.0 -. 5.0 = 0.0")
  (assert-equals (-. 5.0 7.0) -2.0 "5.0 -. 7.0 = -2.0"))

(test test-mult-double
  (assert-equals (*. 0.0 0.0) 0.0 "0.0 *. 0.0 = 0.0")
  (assert-equals (*. 7.0 1.0) 7.0 "7.0 *. 1.0 = 7.0")
  (assert-equals (*. 7.0 -7.0) -49.0 "7.0 *. -7.0 = -49.0"))

(define *Infinity* (/. 1.0 0.0))
(define *NaN* (%. 1.0 0.0))

(test test-div-double
  (assert-equals (/. 0.0 1.0) 0.0 "0.0 /. 1.0 = 0.0")
  (assert-equals (/. 8.0 2.0) 4.0 "8.0 /. 2.0 = 4.0")
  (assert-equals (/. 9.0 2.0) 4.5 "9.0 /. 2.0 = 4.0")
  (assert-equals (/. 5.0 0.0) *Infinity* "5.0 /. 0.0 = Infinity"))

(test test-div2-double
  (assert-equals (div. 0.0 1.0) 0.0 "0.0 div. 1.0 = 0.0")
  (assert-equals (div. 8.0 2.0) 4.0 "8.0 div. 2.0 = 4.0")
  (assert-equals (div. 9.0 2.0) 4.5 "9.0 div. 2.0 = 4.0")
  (assert-equals (div. 5.0 0.0) *Infinity* "5.0 div. 0.0 = Infinity"))

(test test-mod-double
  (assert-equals (%. 5.0 1.0) 0.0 "anything mod. 1.0 = 0.0")
  (assert-equals (%. 5.0 2.0) 1.0 "5.0 %. 2.0 = 1.0")
  (assert-equals (%. 5.0 3.0) 2.0 "5.0 %. 3.0 = 2.0")
  (assert-equals (%. -5.0 3.0) -2.0 "-5.0 %. 3.0 = -2.0")
  (assert-equals (%. 5.0 -3.0) 2.0 "5.0 %. -3.0 = 2.0")
  (assert-equals (%. -5.0 -3.0) -2.0 "-5.0 %. -3.0 = -2.0")
  (assert-equals (%. 5.0 0.0) *NaN* "5.0 %. 0.0 = NaN"))

(test test-mod2-double
  (assert-equals (mod. 5.0 1.0) 0.0 "anything mod. 1.0 = 0.0")
  (assert-equals (mod. 5.0 2.0) 1.0 "5.0 mod. 2.0 = 1.0")
  (assert-equals (mod. 5.0 3.0) 2.0 "5.0 mod. 3.0 = 2.0")
  (assert-equals (mod. -5.0 3.0) -2.0 "-5.0 mod. 3.0 = -2.0")
  (assert-equals (mod. 5.0 -3.0) 2.0 "5.0 mod. -3.0 = 2.0")
  (assert-equals (mod. -5.0 -3.0) -2.0 "-5.0 mod. -3.0 = -2.0")
  (assert-equals (mod. 5.0 0.0) *NaN* "5.0 mod. 0.0 = NaN"))

(test test-min-double
  (assert-equals (min. 1.0 2.0) 1.0 "min. 1.0 2.0 = 1.0")
  (assert-equals (min. 2.0 1.0) 1.0 "min. 2.0 1.0 = 1.0")
  (assert-equals (min. 1.0 1.0) 1.0 "min. 2.0 1.0 = 1.0"))

(test test-max-double
  (assert-equals (max. 1.0 2.0) 2.0 "max. 1.0 2.0 = 2.0")
  (assert-equals (max. 2.0 1.0) 2.0 "max. 2.0 1.0 = 2.0")
  (assert-equals (max. 1.0 1.0) 1.0 "max. 2.0 1.0 = 1.0"))

(test test-int-comparison
  (assert-true (= 5 5) "5 = 5")
  (assert-false (= 5 0) "not 5 = 0")
  (assert-true (!= 5 0) "5 != 0")
  (assert-false (!= 5 5) "not 5 != 5")
  (assert-true (< 1 5) "1 < 5")
  (assert-false (< 5 1) "not 5 < 1")
  (assert-true (<= 1 1) "1 <= 1")
  (assert-true (<= 1 5) "1 <= 5")
  (assert-false (<= 5 1) "not 5 <= 1")
  (assert-true (>= 1 1) "1 >= 1")
  (assert-true (>= 5 1) "5 >= 1")
  (assert-false (>= 1 5) "not 1 >= 5"))

(test test-double-comparison
  (assert-true (=. 5.0 5.0) "5.0 =. 5.0")
  (assert-false (=. 5.0 0.0) "not 5.0 =. 0.0")
  (assert-true (!=. 5.0 0.0) "5.0 !=. 0.0")
  (assert-false (!=. 5.0 5.0) "not 5.0 !=. 5.0")
  (assert-true (<. 1.0 5.0) "1.0 <. 5.0")
  (assert-false (<. 5.0 1.0) "not 5.0 <. 1.0")
  (assert-true (<=. 1.0 1.0) "1.0 <=. 1.0")
  (assert-true (<=. 1.0 5.0) "1.0 <=. 5.0")
  (assert-false (<=. 5.0 1.0) "not 5.0 <=. 1.0")
  (assert-true (>=. 1.0 1.0) "1.0 >=. 1.0")
  (assert-true (>=. 5.0 1.0) "5.0 >=. 1.0")
  (assert-false (>=. 1.0 5.0) "not 1.0 >=. 5.0"))

(test test-int-neg
  (assert-equals (neg 5) -5 "neg 5 = -5")
  (assert-equals (neg 0) 0 "neg 0 = 0")
  (assert-equals (neg -5) 5 "neg -5 = 5"))

(test test-double-neg
  (assert-equals (neg. 5.0) -5.0 "neg. 5.0 = -5.0")
  (assert-equals (neg. 0.0) -0.0 "neg. 0.0 = -0.0")
  (assert-equals (neg. -0.0) 0.0 "neg. -0.0 = 0.0")
  (assert-equals (neg. -5.0) 5.0 "neg. -5.0 = 5.0"))

(test test-conversions
  (assert-equals (int->char 65) #\A "int 65 = char A")
  (assert-equals (int->char 97) #\a "int 97 = char a")
  (assert-equals (char->int #\B) 66 "char B = int 66")
  (assert-equals (char->int #\b) 98 "char b = int 98")
  (assert-equals (int->double 12) 12.0 "int 12 = double 12.0")
  (assert-equals (int->double 0) 0.0 "int 0 = double 0.0")
  (assert-equals (double->int 12.0) 12 "double 12.0 = int 12")
  (assert-equals (double->int -0.0) 0 "double -0.0 = int 0")
  (assert-equals (double->int 5.5) 5 "double 5.5 = int 5"))

(test test-boolean-ops
  (assert-true (and #t #t) "and #t #t is true")
  (assert-false (and #t #f) "and #t #f is false")
  (assert-false (and #f #t) "and #f #t is false")
  (assert-false (and #f #f) "and #f #f is false")

  (assert-true (or #t #t) "or #t #t is true")
  (assert-true (or #t #f) "or #t #f is true")
  (assert-true (or #f #t) "or #f #t is true")
  (assert-false (or #f #f) "or #f #f is false")

  (assert-false (xor #t #t) "xor #t #t is false")
  (assert-true (xor #t #f) "xor #t #f is true")
  (assert-true (xor #f #t) "xor #f #t is true")
  (assert-false (xor #f #f) "xor #f #f is false")

  (assert-true (not #f) "not #f is true")
  (assert-false (not #t) "not #t is false"))

(test test-bit-ops
  (assert-equals (& 3 3) 3 "3 & 3 = 3")
  (assert-equals (& 3 1) 1 "3 & 1 = 1")
  (assert-equals (& 3 2) 2 "3 & 2 = 2")
  (assert-equals (& 3 4) 0 "3 & 4 = 0")
  (assert-equals (| 3 0) 3 "3 | 0 = 3")
  (assert-equals (| 3 4) 7 "3 | 4 = 7")
  (assert-equals (| 4 2) 6 "4 | 2 = 6")
  (assert-equals (^ 5 2) 7 "5 ^ 2 = 7")
  (assert-equals (^ 5 4) 1 "5 ^ 4 = 1")
  (assert-equals (^ 5 5) 0 "5 ^ 5 = 0")
  (assert-equals (~ 5) -6 "~5 = -6")
  (assert-equals (~ -6) 5 "~ -6 = 5")
  (assert-equals (<< 1 5) 32 "1 << 5 = 32")
  (assert-equals (<< 5 0) 5 "5 << 0 = 5")
  (assert-equals (<< 0 5) 0 "0 << 5 = 0")
  (assert-equals (>> 96 5) 3 "96 >> 5 = 3")
  (assert-equals (>> 96 7) 0 "96 >> 7 = 0")
  (assert-equals (>> 96 0) 96 "96 >> 0 = 96")
  (assert-equals (>> 0 5) 0 "0 >> 5 = 0"))

(test test-to-string
  (assert-equals (->string 5) "5" "5 ->string = 5")
  (assert-equals (->string 5.0) "5.0" "5.0 ->string = 5.0")
  (assert-equals (->string (list 1 2 3)) "(1 2 3)" "(1 2 3) ->string = (1 2 3)")
  (assert-equals (->string #t) "true" "#t ->string = true")
  (assert-equals (->string #f) "false" "#f ->string = false")
  (assert-equals (->string "quux") "quux" "quux -> string = quux"))

(test test-id
  (assert-equals (id 5) 5 "id 5 = 5")
  (assert-equals (id 5.0) 5.0 "id 5.0 = 5.0")
  (assert-equals (id #t) #t "id #t = #t")
  (assert-equals (id "quux") "quux" "id quux = quux")
  (assert-equals (id (list 1 2 3)) (list 1 2 3) "id (1 2 3) = (1 2 3)")
  (assert-equals (id #\A) #\A "id #\A = #\A"))

(test test-string-list
  (assert-equals (list->string (list #\A #\B #\C)) "ABC" "(#\A #\B #\C) to string = ABC")
  (assert-equals (list->string nil) "" "nil to string = ")
  (assert-equals (string->list "") nil "empty string to list = nil")
  (assert-equals (string->list "abc") (list #\a #\b #\c) "abc to list = (#\a #\b #\c)"))

(test test-equals
  (assert-true (equals? 5 5) "5 equals 5")
  (assert-true (equals? "foo" "foo") "foo equals foo")
  (assert-true (equals? (list 1 2 3) (list 1 2 3)) "(1 2 3) equals (1 2 3)")
  (assert-false (equals? 5 6) "not 5 equals 6")
  (assert-false (equals? "foo" "bar") "not foo equals bar")
  (assert-false (equals? (list 1 2 3) (list 4 5 6)) "not (1 2 3) equals (4 5 6)"))

(test test-cons
  (assert-equals (cons 1 (cons 2 (cons 3 nil))) (list 1 2 3) "cons 1 cons 2 cons 3 nil = (1 2 3)")
  (assert-equals (cons 1 nil) (list 1) "cons 1 nil = (1)"))

(test test-to-list
  (assert-equals (->list 1) (list 1) "1 ->list = (1)")
  (assert-equals (->list 2.0) (list 2.0) "2.0 ->list = (2.0)")
  (assert-equals (->list #f) (list #f) "#f ->list = (#f)")
  (assert-equals (->list (list 1 2 3)) (list (list 1 2 3)) "(1 2 3) ->list = ((1 2 3))")
  (assert-equals (->list nil) (list (Nil)) "nil -> list = (Nil)"))

(test test-range
  (assert-equals (range 1 5) (list 1 2 3 4 5) "range 1 5 = (1 2 3 4 5)")
  (assert-equals (range 5 1) nil "range 5 1 = nil")
  (assert-equals (length (range 1 5000)) 5000 "length (range 1 5000) = 5000"))

(test test-head-tail
  (assert-equals (head (list 1 2 3)) 1 "head (1 2 3) = 1")
  (assert-equals (head (cons 1 nil)) 1 "head cons 1 nil = 1")
  (assert-equals (tail (list 1 2 3)) (list 2 3) "tail (1 2 3) = (2 3)")
  (assert-equals (tail (list 1)) nil "tail (1) = nil")
  (assert-equals (tail nil) nil "tail nil = nil")
  (expect-exception FailException "head nil should fail"
    (head nil)))

(test test-empty-nil
  (assert-true (empty? nil) "nil is empty")
  (assert-true (nil? nil) "nil is nil")
  (assert-true (empty? (tail (cons 1 nil))) "tail cons 1 nil is empty")
  (assert-true (nil? (tail (cons 1 nil))) "tail cons 1 nil is nil")
  (assert-false (empty? (list 1 2 3)) "(1 2 3) is not empty")
  (assert-false (nil? (list 1 2 3)) "(1 2 3) is not nil")
  (assert-false (empty? (cons 1 nil)) "cons 1 nil is not empty")
  (assert-false (nil? (cons 1 nil)) "cons 1 nil is not nil"))

(test test-fail
  (expect-exception FailException "fail should fail"
    (fail "oops")))

(test test-split-join
  (assert-equals (split "foo bar baz" " ") (list "foo" "bar" "baz") "foo bar baz splits to (foo bar baz)")
  (assert-equals (join (list "foo" "bar" "baz") ",") "foo,bar,baz" "(foo bar baz) joins with , to foo,bar,baz")
  (assert-equals (join (split "foo bar baz" " ") " ") "foo bar baz" "foo bar baz split + join to foo bar baz"))