(display (cons (cons 2 3) (cons 4 5)))
(newline)
(define (make-seg start-p end-p)
  (cons start-p end-p))

(define (start-seg seg)
  (car seg))

(define (end-seg seg)
  (cdr seg))

(define (make-p x y)
  (cons x y))

(define (x-p p)
  (car p))

(define (y-p p)
  (cdr p))

(define (print-p p)
  (newline)
  (display "(")
  (display (x-p p))
  (display ",")
  (display (y-p p))
  (display ")"))

;;(print-p (make-p 11 2))

(define (midpoint seg)
  (make-p (/ (+ (x-p (start-seg seg)) (x-p (end-seg seg))) 2)
          (/ (+ (y-p (start-seg seg)) (y-p (end-seg seg))) 2)))

(define start (make-p 1 1))
(define end (make-p 3 3))
(define seg (make-seg start end))
(display seg)
(print-p (midpoint seg))
(newline)

(define (make-rect p1 p2 p3 p4)
  (cons p1 (cons p2 (cons p3 p4))))

(define (get-point-n rect n)
  (cond ((= n 1) (car rect))
        ((= n 2) (car (cdr rect)))
        ((= n 3) (car (cdr (cdr rect))))
        ((= n 4) (cdr (cdr (cdr rect))))
        (else (error (format "A rectangle has 4 points, "%s" is not one of them." n)))
        ))

(define arect (make-rect (make-p 1 1)
                          (make-p 1 -1)
                          (make-p -1 -1)
                          (make-p -1 1)))

(print-p (get-point-n arect 1))
(print-p (get-point-n arect 2))
(print-p (get-point-n arect 3))
(print-p (get-point-n arect 4))
(newline)
