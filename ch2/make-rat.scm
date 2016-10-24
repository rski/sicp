(define (make-rat n d)
;; The * n d is a bit of a hack, and requires more computation than necessary
;; Is there a better way?
  (if (> (* n d) 0) (normalise-cons (abs n) (abs d))
 	(normalise-cons (- (abs n)) (abs d))))
(define (normalise-cons a b)
  (let ((g (gcd a b)))
	(cons (/ a g) (/ b g))))
(make-rat 1 2)
(make-rat (- 1) 2)

