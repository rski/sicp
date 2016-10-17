(define tolerance 0.0001)
(define (fixed-point f initial-guess)
  (define (close-enough v1 v2)
    (display v1)
    (display "\n")
    (< (abs (- v1 v2)) tolerance)
    )
  (define (iter guess)
    (let ((next (f guess)))
     (if (close-enough guess next) next
        (iter next))
    ))
  (iter initial-guess))


(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

(fixed-point (lambda (x) (/ (log10 1000) (log10 x))) 2.0)
;; w/ avg damp
(display "\nAvg damp\n")
(define (average a b)
  (* (/ 1.0 2.0) (+ a b))
  )
(define (avg-damp-x^x)
  (define (f x) (/ (log10 1000) (log10 x)))
  (fixed-point (lambda (x) (average (f x) x)) 7.0))

(avg-damp-x^x)
