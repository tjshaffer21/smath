(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun erathosthenes-sieve (n)
  "Sieve of Erathosthenes algorithm.

  Parameter
    n : int : 1 < N < array-total-size-limit
  Return
    list
    nil : invalid input
  Error
    type-error : N is not an integer."
  (declare (type integer n))

  (when (or (<= n 1) 
            (>= n array-total-size-limit))
    (return-from erathosthenes-sieve nil))
    
  (let ((primes (make-array (1+ n) :element-type 'bit :initial-element 1)))
    (setf (aref primes 0) 0)
    (setf (aref primes 1) 0)
    (iterate:iter
      (iterate:for i iterate::from 3)
      (iterate:until (> (* i i) n))
      (when (= (aref primes i) 1)
        (iterate:iter
          (iterate:for k iterate::initially (* i i) iterate::then (+ k i))
          (iterate:while (<= k n))
          (setf (aref primes k) 0)))
      (iterate:finally (iterate::return
        (append (list 2) 
                (iterate:iter
                    (iterate:for i iterate::from 3 iterate::to n iterate::by 2)
                    (when (= (bit primes i) 1)
                      (iterate::collect i)))))))))

(defun sundaram-sieve (n)
  "Sieve of Sundaram algorithm.

  Parameters
    n : integer : 1 < n < (array-total-size-limit+2) * 2
  Return
    list
    nil : invalid input
  Error
    type-error : N is not an integer."
  (declare (type integer n))

  (let ((adjust (floor (- n 1) 2)))
    (when (or (<= adjust 1) (>= adjust array-total-size-limit))
      (return-from sundaram-sieve nil))
    (iterate:iter
      (iterate:with sieve = (make-array (1+ adjust) :element-type 'bit :initial-element 1))
      (iterate:for i iterate::from 1 iterate::to adjust)
      (iterate:iter
        (iterate:for j iterate::initially 1 iterate::then (1+ j))
        (iterate:for k = (+ i j (* 2 i j)))
        (iterate:while (<= k adjust))
        (setf (bit sieve k) 0))
      (iterate:finally
        (iterate::return
          (append (list 2)
                  (iterate:iter
                    (iterate:for c iterate::initially 1 iterate::then (1+ c))
                    (iterate:while (<= c adjust))
                    (when (= (bit sieve c) 1)
                      (iterate::collecting (1+ (* 2 c)))))))))))