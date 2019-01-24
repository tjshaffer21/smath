(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defvar *fib-cache* nil)

(defun binet-fib (n)
  "Use Binet's Formula to find the Fibonacci value of integer N. Returnig an
 integer value."
  (declare (type integer n))
  (let ((sq5 2.23606797749979d0)         ; sqrt(5)
        (phi 1.61803398874989484820d0)   ; (1+sq(5)) / 2
        (psi -0.61803398874989484820d0)) ; (1-sq(5)) / 2
    (round (- (expt (the long phi) (the fixnum n))
              (expt (the long psi) (the fixnum n)))
            (the long sq5)))) ; (1/sq5)  * ((phi**n) - (psi**n)

(defun fibonacci (n)
  "Return the fibonacci value for integer N. Where N must be a postive value
 greater than 0. If N <= 0 then NIL is returned; else an integer value is re-
 turned. *FIB-CACHE* is modified if N has not been found previously."
  (declare (type integer n))
  (when (<= n 0) (return-from fibonacci nil))

  (cond (*fib-cache*
          (when (>= n (array-total-size *fib-cache*))
            (adjust-array *fib-cache* (+ (array-total-size *fib-cache*) n))))
        (t (setf *fib-cache* (make-array (if (= n 1) 2 n)
                                  :element-type 'integer
                                  :initial-element 0
                                  :adjustable t))
           (setf (aref *fib-cache* 0) 1)
           (setf (aref *fib-cache* 1) 1)))

  (if (= (aref *fib-cache* (1- n)) 0)
      (setf (aref *fib-cache* (1- n)) (binet-fib n))
      (aref *fib-cache* (1- n))))

;;; Example
;;; (sum-fibonacci 34 :even) ->  4613732
;;; (sum-fibonacci 34 :odd)  -> 10316619
;;; (sum-fibonacci 34)       -> 14930351
(defun sum-fibonacci (n &optional (key nil))
  "Sum the values of fibonacci up to N inclusive based on the keyword KEY; where
 N must be greater than 0. KEY is either :even, :odd, or nil (total sum). If N
 is invalid then nil is returned else an integer is returned."
  (declare (type integer n))
  (iterate:iter
    ;; Every third term is even, so optimize by setting even's step size to 3.
    (iterate:for sum iterate::from (if (eq key :even) 3 1) iterate::to n
        iterate::by (if (eq key :even) 3 1))
    (iterate:for fib = (fibonacci sum))
    (iterate::summing (if (eq key :odd)
                          (if (oddp fib) fib 0)
                          fib))))

;;; Example
;;; (even-sum-fibonacci 4000000) -> 4613732
(defun even-sum-fibonacci (n)
  "Calculate the sum of even fibonacci values up to the integer N returning the
 resulting integer."
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