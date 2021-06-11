(match (list 1 2 3)
  ((Cons 1 (Cons 1 _)) (print "baz\n"))
  ((Cons 1 (Cons 3 _)) (print "bar\n"))
  ((_ 2 _) (print "foo\n")))
