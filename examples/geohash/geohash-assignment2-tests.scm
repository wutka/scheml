(define (assert-equals a b)
  (if (equals? a b) (print "PASSED\n")
      (printf "Test failed: %s does not equal %s\n" a b)))

(define (assert-true a)
  (if a (print "PASSED\n")
      (progn
        (print "Test failed\n"))))

(define (test-simple-insert)
  (let* ((db (make-geodb 16))
         ((Pair _ db2) (geodb-insert 0.0 0.0 db)))
    (map (lambda (i)
           (assert-true (geodb-contains 0.0 0.0 i db2)))
         (range 0 15))))

(define (test-simple-delete)
  (let* ((db (make-geodb 16))
         ((Pair _ db2) (geodb-insert 0.0 0.0 db))
         ((Pair _ db3) (geodb-delete 0.0 0.0 db2)))
    (map (lambda (i)
           (assert-true (not (geodb-contains 0.0 0.0 i db3))))
         (range 0 15))))

(define (test-zero-bits)
  ;;; Use geodb-chain to queue up several inserts.
  ;;; The geodb-insert function takes the database as the
  ;;; last parameter, so each of these calls to geodb-insert
  ;;; are partial functions, and the geodb-chain function will
  ;;; invoke them with the updated version of the GeoDB
  (let ((db (geodb-chain (make-geodb 16)
                (list (geodb-insert 0.0 0.0)
                      (geodb-insert 90.0 180.0)
                      (geodb-insert -90.0 -180.0)
                      (geodb-insert -90.0 180.0)
                      (geodb-insert 90.0 -180.0)))))
    (assert-equals 5 (length (geodb-nearby 0.0 0.0 0 db)))))

(define (test-zero-bits-delete)
  (let* ((db (geodb-chain (make-geodb 16)
                 (list (geodb-insert 0.0 0.0)
                       (geodb-insert 90.0 180.0)
                       (geodb-insert -90.0 -180.0)
                       (geodb-insert -90.0 180.0)
                       (geodb-insert 90.0 -180.0))))
         ((Pair _ db2) (geodb-delete-all 0.0 0.0 0 db)))
    (assert-equals 0 (length (geodb-nearby 0.0 0.0 0 db2)))))

(define (test-insert-delete-series)
  (let ((db (geodb-chain (make-geodb 16)
                (list (geodb-insert 0.0 0.0)
                      (geodb-insert 90.0 180.0)
                      (geodb-insert -90.0 -180.0)
                      (geodb-insert -90.0 180.0)
                      (geodb-insert 90.0 -180.0)))))
    (assert-true (geodb-contains 0.0 0.0 16 db))
    (assert-true (geodb-contains 90.0 180.0 16 db))
    (assert-true (geodb-contains -90.0 -180.0 16 db))
    (assert-true (geodb-contains -90.0 180.0 16 db))
    (assert-true (geodb-contains 90.0 -180.0 16 db))
    (assert-true (geodb-contains 90.5 -180.5 16 db))
    (assert-true (not (geodb-contains 1.0 -1.0 16 db)))
    (assert-true (not (geodb-contains 45.0 -45.0 16 db)))
    ;;; Begin a slightly-ugle chain of lets to do some updates and
    ;;; assertions
    (progn
      (:= (Pair _ db-deleted1) (geodb-delete 90.0 -180.0 db))
      (assert-true (not (geodb-contains 90.0 -180.0 16 db-deleted1)))
      (:= (Pair _ db-deleted-all) (geodb-delete-all 1.0 1.0 1 db-deleted1))
      (assert-true (geodb-contains -90.0 -180.0 16 db-deleted-all))
      (assert-true (not (geodb-contains 90.0 180.0 16 db-deleted-all)))
      (:= (Pair _ db-inserted) (geodb-insert 90.0 180.0 db-deleted-all))
      (assert-true (geodb-contains 90.0 180.0 16 db-inserted)))))

;;; Run all the tests
(define (test-suite)
  (test-simple-insert)
  (test-simple-delete)
  (test-zero-bits)
  (test-zero-bits-delete)
  (test-insert-delete-series))
