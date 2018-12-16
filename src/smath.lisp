(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun digits (n)
  "Return the number of digits in the non-negative integer N.

  Assumption: N is base 10.
  Note: INDEX is 1-based, right to left.

  Examples
    (integer-at 1234 1) := 4
    (integer-at 1234 4) := 1
  Parameters
    N : integer : N > 0.
  Return
    integer
  Error
    type-error"
  (declare (type integer n))
  (if (= n 0) 1 (floor (1+ (log n 10)))))

(defun integer-at (n index)
  "Return the integer at the INDEX position of the integer N.

  Parameters
    N : integer : N >= 0
    INDEX : integer : INDEX > 1
  Return
    integer
    nil: N < 0, 0 <= INDEX < (digits n)
  Error
    type-error"
  (declare (type integer n index))

  (when (or (< n 0) (<= index 0) (> index (digits n)))
    (return-from integer-at nil))
  (floor (mod (/ n (expt 10 (1- index))) 10)))

(defun integer-into-sequence (n)
  "Return a sequence of the integer N.

  Parameters
    N : integer : N >= 0
  Return
    sequence
  Error
    type-error"
  (declare (type integer n))

  (when (< n 0) (return-from integer-into-sequence nil))

  (let* ((size (floor (1+ (log n 10))))
         (integers (make-array size)))
    (when (= size 1)
      (return-from integer-into-sequence
        (make-array 1 :initial-element n :element-type 'integer)))

    (iterate:iter
      (iterate:for j iterate::from 0)
      (iterate:for i iterate::from (1- size) iterate::downto 0)
      (setf (aref integers j) (integer-at n (1+ i))))
    integers))

(defun multiples (number limit)
  "Get the multiples of the given number up to, and including, limit.

  Parameters
    number : int : NUMBER > 0
    limit  : int
  Return
    list
    nil : if number <= 0.
  Error
    type-error : NUMBER, LIMIT are not integers."
  (declare (type integer number limit))
  (unless (<= number 0)
    (iterate::iter
      (iterate::for i iterate::from (1+ number) iterate::to limit)
      (if (= (mod i number) 0) (iterate::collect i)))))

(defun reverse-integer (n)
  "Reverse the integer N.

  Parameters
    n : integer
  Return
    integer
  Error
    type-error"
  (declare (type integer n))

  (when (and (>= n -9) (<= n 9)) (return-from reverse-integer n))
  (labels ((nrev (x res)
            (if (= x 0)
                res
                (multiple-value-bind (m n)
                  (floor x 10)
                  (nrev m (+ (* res 10) n))))))
    (let ((result (nrev (abs n) 0)))
      (if (< n 0) (- result) result))))