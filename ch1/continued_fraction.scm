(define (fraction nf df k)
  (define (iter n)
    (if (> n k) 0
        (/ (nf n) (+ (df n) (iter (+ 1 n)))))
    )
  (iter 1))

(define (fraction-iterative nf df k)
  (define (iter n acc)
    (if (< n 1) acc
        (iter (- n 1) (/ (nf n) (+ acc (df n))))))
  (iter k 0))

(display "--fraction--\n")
(display (fraction (lambda (x) 1.0)
                   (lambda (x) 1.0)
                   5))
(display "\n")

(display (fraction-iterative (lambda (x) 1.0)
                             (lambda (x) 1.0)
                             5))

(display "\n")
(display "--e--\n")

(define (calc-e k frac)
  (define (d x)
    (if (= (remainder x 3) 2) (expt 2 (+ 1 (quotient x 3)))
        1))
  (+ 2 (frac (lambda (x) 1)
             d
             k)))

(display (/ (calc-e 10 fraction) 1.0))
(display "\n")
(display (/ (calc-e 10 fraction-iterative) 1.0))
(display "\n")

(define (odd x)
  (- (* 2 x) 1))

(define (tan x decimal-points)
  (let ((n (lambda (y) (if (= y 1) x
                           (- (expt x 2))))))
    (fraction n odd decimal-points)))

(define (tan-it x decimal-points)
  (let ((n (lambda (y) (if (= y 1) x
                           (- (expt x 2))))))
    (fraction-iterative n odd decimal-points)))

(display "---tan---\n")
(display (tan 1.0 112))
(display "\n")
(display (tan-it 1.0 11))
