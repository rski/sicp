;; 2.7: Implement the selectors
(define (make-interval a b) (cons a b))
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))


;; Given by book
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


;; 2.8: Implement subtract

(define (sub-interval x y)
  (make-interval (- (upper-bound x) (upper-bound y))
                 (- (lower-bound x) (lower-bound y))))


;; 2.9 Show width is a fun of widths for sums and diff, not true for mult/division
;; w(u1 l1) = (u1 + l1)/2 w(u2 l2)=(u2 +l2)/2 w(u3 l3)=w(u1 + u2 l1 + l2)= (u1 + u2 + l1 + l2)/2 so just sum
;; same for difference
;; for mult,consider (3 2) and (-1 -2).


;; 2.10 Ben Bitdiddle divide by interval that spans over 0
(define (div-interval-zero x y)
  (if (and (<= (upper-bound y) 0) (>= (lower-bound y) 0)) error
      (div-interval x y)))


;; 2.11 Ben cryptically comments:``By testing the signs of the endpoints of the intervals, it is possible to break mul-interval into nine cases, only one of which requires more than two multiplications.'' Rewrite this procedure using Ben's suggestion.
;; welp


;; 2.12 constructor make-center-percent that takes a center and percentage tolerance and produces the interval
;; selector percent that gives percentage of a given interval


;; 2.13
;; Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.


;; 2.14
;; Demonstrate that Lem is right. Investigate the behavior of the system on a variety of arithmetic expressions. Make some intervals A and B, and use them in computing the expressions A/A and A/B. You will get the most insight by using intervals whose width is a small percentage of the center value. Examine the results of the computation in center-percent form

;; 2.15
;; Eva Lu Ator, another user, has also noticed the different intervals computed by different but algebraically equivalent expressions. She says that a formula to compute with intervals using Alyssa's system will produce tighter error bounds if it can be written in such a form that no variable that represents an uncertain number is repeated. Thus, she says, par2 is a ``better'' program for parallel resistances than par1. Is she right? Why?

;; 2.16
;; Warning: this is hard, lolnope
