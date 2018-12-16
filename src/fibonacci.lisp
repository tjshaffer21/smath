(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defvar *fib-cache* nil)

(defun fibonacci (n)
  "Return the fibonacci value for N.

  Parameters
    N : integer : N > 0
  Return
    integer
    nil : N <= 0
  Error
    type-error"
  (declare (type integer n))

  (when (<= n 0) (return-from fibonacci nil))
  (if *fib-cache*
      (when (>= n (array-total-size *fib-cache*))
        (adjust-array *fib-cache* (+ (array-total-size *fib-cache*) n)))
      (progn
        (setf *fib-cache* (make-array (if (= n 1) 2 1)
                              :element-type 'integer
                              :initial-element 0
                              :fill-pointer 2
                              :adjustable t))
        (setf (aref *fib-cache* 0) 1)
        (setf (aref *fib-cache* 1) 1)))

  (if (and (not (= n 0)) (= (aref *fib-cache* (1- n)) 0))
      (iterate:iter
        (iterate:for i iterate::from (fill-pointer *fib-cache*) iterate::to (1- n))
          (setf (aref *fib-cache* i)
                (+ (aref *fib-cache* (1- i)) (aref *fib-cache* (- i 2))))
        (iterate:finally
          (setf (fill-pointer *fib-cache*) i)
          (iterate::return (aref *fib-cache* (1- i)))))
      (aref *fib-cache* (1- n))))

(defun even-sum-fibonacci (n)
  "Calculate the sum of even fibonacci terms using the recurence equation.

  Parameters
    n : integer
  Return
    integer
  Error
    type-error"
  (iterate:iter
    (iterate:for even-1 iterate::initially 0 iterate::then even-2)
    (iterate:for even-2 iterate::initially 2 iterate::then even-3)
    (iterate:for even-3 iterate::initially 0 iterate::then (+ (* 4 even-2) even-1))
    (iterate:for sum iterate::initially 0 iterate::then (+ sum even-2))
    (iterate:while (< even-2 n))
    (when (> even-3 n) (iterate:finish))
    (iterate:finally (iterate::return sum))))

;;; Helpers

(defun drop-fib-cache ()
  (setf *fib-cache* nil))