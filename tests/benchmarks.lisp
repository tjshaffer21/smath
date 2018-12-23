(in-package #:smath.tests)

(defvar *timer* (benchmark:make-timer))

(defmacro do-benchmark-report (sample-size &rest body)
  `(progn
    (benchmark:reset *timer*)
    (dotimes (i ,sample-size t)
      (benchmark:with-sampling (*timer*)
        ,@body))
    (benchmark:report *timer*)))

(defun benchmark-erathosthenes-sieve (random-sample)
  (benchmark:reset *timer*)
  (dotimes (i random-sample t)
    (benchmark:with-sampling (*timer*)
      (smath:erathosthenes-sieve (+ (random 100000) 1000000))))
  (benchmark:report *timer*))

(defun benchmark-sundaram-sieve (random-sample)
  (benchmark:reset *timer*)
  (dotimes (i random-sample t)
    (benchmark:with-sampling (*timer*)
      (smath:sundaram-sieve (+ (random 100000) 10000))))
  (benchmark:report *timer*))