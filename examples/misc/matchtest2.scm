(define (matchlist l)
  (match l
    ((1 1 _) (print "baz\n"))
    ((1 3 _) (print "bar\n"))
    ((_ 2 _) (print "foo\n"))
    ((_) (print "length one\n"))
    ((_ _) (print "length two\n"))
    (nil (print "quux\n"))
    (_ (print "everything else\n"))))
