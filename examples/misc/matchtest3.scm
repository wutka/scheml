(type bool3 (Bool3 bool bool bool))

(match (Bool3 #t #f #t)
       ((Bool3 #t #f _) (print "1\n"))
       ((Bool3 #f _ #t) (print "3\n"))
       ((Bool3 #f #f #f) (print "4\n"))
       ((Bool3 _ #t #f) (print "2\n"))
       ((Bool3 #t #t #t) (print "5\n")))
