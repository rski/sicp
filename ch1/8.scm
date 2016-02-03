(define (abs x)
  (if (< x 0)
     (- x)
     x))

(define (avg y x)
  (/ (+ y x) 2))

(define (sq x)
  (* x x))

(define (good-enough? prevy y)
  (< (abs (- prevy y)) (* 0.001 y)))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))

(define (sqrt-iter prevy y x)
  (if (good-enough? prevy y) y
      (sqrt-iter y (improve y x) x)))

(define (improve y x)
  (avg y (/ x y)))


(define (cube-root x)
  (cube-iter 0.0 1.0 x))

(define (cube-improve y x)
  (/ (+ (/ x (sq y)) (* 2 y))
     3))

(define (cube-iter prevy y x)
  (if (good-enough? prevy y) y
      (cube-iter y (cube-improve y x) x)))

(cube-root 100.0)
