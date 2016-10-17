(define (product transform next a b)
  (if (> a b) 1
      (* (transform a) (product transform next (next a) b))))

(define (id a) a)

(define (plus1 a)
  (+ a 1))

(define (sq a)
  (* a a))

(product sq plus1 1 4)

(define (fact a)
  (if (= a 0) 1
  (product id plus1 1 a))
  )

(fact 4)
(fact 7)
(fact 1)
(fact 0)


(define (pi n)
  (define (pi-transform1 a)
    (* a a))
  (define (pi-transform2 a)
    (* a (+ a 2)))
  (define (pi-next a)
    (+ 2 a))
  (* 4.0 (/ (product pi-transform2 pi-next 2.0 n)
          (product pi-transform1 pi-next 3.0 (+ 1 n))))
  )


(display "\n")
(display (pi 3))
(display "\n")
(display (pi 4))
(display "\n")
(display (pi 15))
(display "\n")
(display (pi 50))
(display "\n")
(display (pi 100))


(define (iter-product transform next a b)
  (define (iter a acc)
    (if (> a b) acc
        (iter (next a) (* acc (transform a)))))
  (iter a 1)
  )

(iter-product sq plus1 1 4)
