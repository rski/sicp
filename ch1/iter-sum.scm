(define (sum transform next a b)
  (define (iter a acc)
    (if (> a b) acc
        (iter (next a) (+ (transform a) acc))))
  (iter a 0)
  )


(define (id a)
  a)
(define (plusone a)
  (+ a 1))
(sum id plusone 1 10)
