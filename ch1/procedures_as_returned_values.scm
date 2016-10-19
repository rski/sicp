(define dx 0.001)
(define (smoothen f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (derivative f)
  (lambda (x) (/ (- (f (+ x dx)) (f x))
                 dx
                 )))


(define (newton-transform f)
  (lambda (x) (- x
                 (/ (f x)
                    ((derivative f) x)))))

(define (netwons-method f guess)
  (fixed-point (newton-transform f) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* x x a)
                 (* x b)
                 c)))

(define (double f)
  (lambda (x) (f (f x))))

(define (compose f g)
  (lambda (x) (f (g x))))

((double (lambda (x) (+ 1 x))) 3)

(define (repeated f n)
  (cond ((< n 1) (lambda (x) x))
        ((= n 1) f)
        (else  (compose (f
                         (repeated f (- n 1))
                         )))))

((repeated (lambda (x) (+ 1 x) ) 6) 5)
