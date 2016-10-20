(define dx 0.0001)

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
        (else (compose f (repeated f (- n 1))))))

((repeated (lambda (x) (+ 1 x)) 6) 5)

;; smoothen
(define (smoothen f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (n-smoothen f n)
  (lambda (x)
    ((repeated (smoothen f) n) x)))

;; iterative-improve
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess) guess
        ((iterative-improve good-enough? improve) (improve guess)))))

(define (inc-improve x)
  (define (good-enough? x)
    (= x 15))
  (define (improve x)
    (+ 1 x))
  ((iterative-improve good-enough? improve) x))

(display (inc-improve 0))

(define (fixed-point f guess)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) dx))
  (define (improve x)
    (f x))
  ((iterative-improve good-enough? improve) guess))

(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ (/ x y) y) 2)) 10.0))

(display "\n--sqrt with f-point--\n")
(display (sqrt 9))
(display "\n")
(display (sqrt 100))

(define (average x y)
  (/ (+ x y)
     2))

(define (avg-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nroot x n)
  (define (f y)
    (/ x (expt y (- n 1))))
  (fixed-point ((repeated avg-damp (floor (log2 n))) f) 1.0))

(display (nroot 10000 16))
