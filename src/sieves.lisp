(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun erathosthenes-sieve (number)
  "Sieve of Erathosthenes algorithm.

  Parameter
    number : int : 1 < NUMBER < array-total-size-limit
  Return
    list
    nil : invalid input
  Error
    type-error : NUMBER is not an integer."
  (declare (type integer number)
           (optimize (speed 3) (safety 3) (debug 0)))

  (when (or (<= number 1) (>= number array-total-size-limit))
    (return-from erathosthenes-sieve nil))
  (iterate:iter
    (iterate:with primes = (make-array (1- number) :element-type 'bit
                                                   :initial-element 1))
    (iterate:for i iterate::from 2 iterate::to (ceiling (sqrt number)))
    (when (smath:primep i)
          (iterate:iter
            (iterate:for j iterate:in (multiples i number))
            (setf (bit primes (- j 2)) 0)))
    (iterate:finally
      (iterate::return
        (iterate:iter
          (iterate:for i iterate::from 0 iterate::to (- number 2))
          (when (= (bit primes i) 1) (iterate::collect (+ i 2))))))))

(defun sundaram-sieve (n)
  "Sieve of Sundaram algorithm.

  Parameters
    n : integer : 1 < n < (array-total-size-limit+2) * 2
  Return
    list
    nil : invalid input
  Error
    type-error : N is not an integer."
  (declare (type integer n)
           (optimize (speed 3) (safety 3) (debug 0)))

  (let ((adjust (floor (- n 2) 2)))
    (when (or (<= adjust 1) (>= adjust array-total-size-limit))
      (return-from sundaram-sieve nil))
    (iterate:iter
      (iterate:with sieve = (make-array adjust :element-type 'bit :initial-element 1))
      (iterate:for i iterate::from 1 iterate::to adjust)
      (iterate:iter
        (iterate:for k = (- (+ i j (* 2 i j)) 2))
        (iterate:for j iterate::initially 1 iterate::then (1+ j))
        (iterate:while (< k adjust))
        (setf (bit sieve k) 0))
      (iterate:finally
        (iterate::return
          (append (list 2 3)
                  (iterate:iter
                    (iterate:for c iterate::initially 0 iterate::then (1+ c))
                    (iterate:while (< c adjust))
                    (when (= (bit sieve c) 1)
                      ;; 2n+1; but, c(0..n] -> 2..n
                      (iterate::collecting (1+ (* 2 (+ c 2))))))))))))