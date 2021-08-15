(test make-array-test
  (assert-equals (list->array (range 1 10)) (make-array 1 2 3 4 5 6 7 8 9 10) "list->array != make-array")
  (assert-equals (make-array-with-default 5 #\a) (make-array #\a #\a #\a #\a #\a) "make-array-with-default")
  (assert-equals (make-array-with-function 10 id) (list->array (range 0 9)) "make-array-with-function")
  (assert-equals (list->array (array->list (list->array (range 1 10)))) (make-array 1 2 3 4 5 6 7 8 9 10) "list->array array->list"))

(test array-map-test
  (assert-equals (array-map (+ 5) (make-array 1 2 3 4 5)) (make-array 6 7 8 9 10) "array-map (+ 5)")
  )

(test array-fold-test
  (assert-equals (array-fold + 0 (list->array (range 1 10))) (fold + 0 (range 1 10)) "array-fold should = list fold")
  )

(test array-ref-test
  (let ((fooarr (list->array (range 1 10))))
    (assert-equals 1 (@ fooarr 0) "element 0 in array of 1..10 = 1")
    (assert-equals 2 (@ fooarr 1) "element 1 in array of 1..10 = 2")
    (assert-equals 3 (@ fooarr 2) "element 2 in array of 1..10 = 3")
    (assert-equals 4 (@ fooarr 3) "element 3 in array of 1..10 = 4")
    (assert-equals 5 (@ fooarr 4) "element 4 in array of 1..10 = 5")
    (assert-equals 6 (@ fooarr 5) "element 5 in array of 1..10 = 6")
    (assert-equals 7 (@ fooarr 6) "element 6 in array of 1..10 = 7")
    (assert-equals 8 (@ fooarr 7) "element 7 in array of 1..10 = 8")
    (assert-equals 9 (@ fooarr 8) "element 8 in array of 1..10 = 9")
    (assert-equals 10 (@ fooarr 9) "element 9 in array of 1..10 = 10")
    (expect-exception FailException "negative index should fail"
        (@ fooarr -1))
    (expect-exception FailException "index >= size should fail"
        (@ fooarr 10))
    ))