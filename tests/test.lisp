(in-package #:smath.tests)

(lisp-unit:define-test test-digits
  (lisp-unit:assert-error 'type-error (smath::digits #\A))

  (lisp-unit:assert-eql 1 (smath::digits 0))
  (lisp-unit:assert-eql 1 (smath::digits 1))
  (lisp-unit:assert-eql 4 (smath::digits 1234)))

(lisp-unit:define-test test-even-sum-fibonacci
  (lisp-unit:assert-error 'type-error (smath::even-sum-fibonacci #\A))

  (lisp-unit:assert-eql 188 (smath::even-sum-fibonacci 400)))

(lisp-unit:define-test test-fibonacci
  (lisp-unit:assert-error 'type-error (smath::fibonacci #\A))
  (lisp-unit:assert-equal nil (smath::fibonacci -1))
  (lisp-unit:assert-equal nil (smath::fibonacci 0))

  (lisp-unit:assert-equal 1 (smath::fibonacci 1))
  (lisp-unit:assert-equal 1 (smath::fibonacci 2))
  (lisp-unit:assert-equal 2 (smath::fibonacci 3))
  (lisp-unit:assert-equal 34 (smath::fibonacci 9))
  (lisp-unit:assert-equal 55 (smath::fibonacci 10))
  (lisp-unit:assert-equal 3 (smath::fibonacci 4))
  (lisp-unit:assert-equal 5 (smath::fibonacci 5))
  (lisp-unit:assert-equal 8 (smath::fibonacci 6))
  (lisp-unit:assert-equal 13 (smath::fibonacci 7))
  (lisp-unit:assert-equal 21 (smath::fibonacci 8))
  (lisp-unit:assert-equal 6765 (smath::fibonacci 20)))

(lisp-unit:define-test test-multiples
  (lisp-unit:assert-error 'type-error (smath::multiples #\A 1))
  (lisp-unit:assert-error 'type-error (smath::multiples 1 #\A))

  (lisp-unit:assert-false (smath::multiples -1 5))

  (lisp-unit:assert-equal (list 4 6 8) (smath::multiples 2 8))
  (lisp-unit:assert-equal (list 6 9 12 15 18) (smath::multiples 3 20)))

(lisp-unit:define-test test-integer-at
  (lisp-unit:assert-error 'type-error (smath::integer-at 123 #\C))
  (lisp-unit:assert-error 'type-error (smath::integer-at #\D 1))

  (lisp-unit:assert-eql 4 (smath::integer-at 1234 1))
  (lisp-unit:assert-eq 1 (smath::integer-at 1234 4))
  (lisp-unit:assert-equal 1 (smath::integer-at 1234 4))
  (lisp-unit:assert-equal 4 (smath::integer-at 1234 1))
  (lisp-unit:assert-equal nil (smath::integer-at 1234 0))
  (lisp-unit:assert-equal nil (smath::integer-at 1234 5))
  (lisp-unit:assert-equal nil (smath::integer-at -1234 4)))

(lisp-unit:define-test test-integer-into-sequence
  (lisp-unit:assert-error 'type-error (smath::integer-into-sequence #\A))

  (lisp-unit:assert-false (smath::integer-into-sequence -1))

  (lisp-unit:assert-equalp #(3) (smath::integer-into-sequence 3))
  (lisp-unit:assert-equalp #(3 2 1) (smath::integer-into-sequence 321))
  (lisp-unit:assert-equalp #(1 2 3 4 5 6 7 8 9 0)
                           (smath::integer-into-sequence 1234567890)))

(lisp-unit:define-test test-prime
  (lisp-unit:assert-error 'type-error (smath::primep #\A))

  (lisp-unit:assert-false (smath::primep -1 128))
  (lisp-unit:assert-false (smath::primep 90 128))
  (lisp-unit:assert-false (smath::primep 15 128))

  (lisp-unit:assert-true (smath::primep 2 128))
  (lisp-unit:assert-true (smath::primep 3 128))
  (lisp-unit:assert-true (smath::primep 5 128))
  (lisp-unit:assert-true (smath::primep 7 128))
  (lisp-unit:assert-true (smath::primep 11 128))
  (lisp-unit:assert-true (smath::primep 13 128))
  (lisp-unit:assert-true (smath::primep 23 128))
  (lisp-unit:assert-true (smath::primep 269 128))
  (lisp-unit:assert-true (smath::primep 331 128))
  (lisp-unit:assert-true (smath::primep 1877 128))
  (lisp-unit:assert-true (smath::primep 2729 128))
  (lisp-unit:assert-true (smath::primep 104729 128))

  ; Carmichael numbers
  (lisp-unit:assert-false (smath::primep 561 128))
  (lisp-unit:assert-false (smath::primep 41041 128))
  (lisp-unit:assert-false (smath::primep 340561 128)))

(lisp-unit:define-test test-reverse-integer
  (lisp-unit:assert-error 'type-error (smath::reverse-integer #\A))

  (lisp-unit:assert-equal 987654321 (smath::reverse-integer 123456789))
  (lisp-unit:assert-equal 32 (smath::reverse-integer 23))
  (lisp-unit:assert-equal -32 (smath::reverse-integer -23))
  (lisp-unit:assert-equal 231 (smath::reverse-integer 132)))

(lisp-unit:define-test test-sieve-of-erathosthenes
  (lisp-unit:assert-error 'type-error (smath::erathosthenes-sieve #\A))

  (lisp-unit:assert-false (smath::erathosthenes-sieve -1))
  (lisp-unit:assert-false (smath::erathosthenes-sieve array-total-size-limit))

  (lisp-unit:assert-equal (list 2 3 5 7) (smath::erathosthenes-sieve 8))
  (lisp-unit:assert-equal (list 2 3 5 7 11 13 17 19)
                          (smath::erathosthenes-sieve 20)))

(lisp-unit:define-test test-sieve-of-sundaram
  (lisp-unit:assert-error 'type-error (smath::sundaram-sieve #\A))

  (lisp-unit:assert-false (smath::sundaram-sieve -1))
  (lisp-unit:assert-false
    (smath::sundaram-sieve (* (+ array-total-size-limit 2) 2)))

  (lisp-unit:assert-equal '(2 3 5 7 11 13 17) (smath::sundaram-sieve 17))
  (lisp-unit:assert-equal (list 2 3 5 7 11 13 17 19)
    (smath::sundaram-sieve 20)))
