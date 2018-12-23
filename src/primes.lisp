(in-package #:smath)
(declaim (optimize (speed 3) (safety 3) (debug 0)))

(defun primep (n &optional (k 128))
  "Return if N is a prime using K as a confidence check.

  Paramters
    N : integer : N > 1
    K : integer : N >= 1; default 128.
  Return
    bool
  Error
    type-error"
  (declare (type integer n k))

  (cond ((or (<= n 1) (and (> n 2) (evenp n)))
         (return-from primep nil))
        ((or (= n 2) (= n 3)) (return-from primep t)))

  (iterate:iter
    (iterate:with rd = (iterate:iter
                          (iterate:for r iterate::from 1)
                          (iterate:for d iterate::initially (1- n)
                                         iterate::then (ash d -1))
                          (iterate:while (= (logand d 1) 0))
                          (iterate:finally (iterate::return (list r d)))))
    (iterate:for a = (+ (random (- n 2)) 2))
    (iterate:for x = (mod (expt a (first (last rd))) n))
    (iterate:repeat k)
    (unless (or (= x 1) (= x (1- n)))
      (iterate:iter
        (iterate:repeat (1- (first rd)))
        (setf x (mod (* x x) n))
        (when (= x (1- n)) (iterate:leave))
        (iterate:finally (return-from primep nil)))))
    t)