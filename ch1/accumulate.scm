(define (accumulate combine-op null-val transform next a b)
  (if (> a b) null-val
      (combine-op (transform a) (accumulate combine-op null-val transform next (next a) b))
      )
  )
(define (plus1 a)
  (+ a 1)
  )

(define (id a) a)

(accumulate * 1 id plus1 1 4)
(accumulate * 1 id plus1 1 7)
(accumulate + 0 id plus1 1 5)

(define (filter-acc predicate combine-op null-val transform next a b)
  (define (filter val)
    (if (predicate val) val
        null-val))
  (if (> a b) null-val
      (combine-op (filter (transform a)) (filter-acc predicate combine-op null-val transform next (next a) b))))

(define (true-p a)
  #t)


(filter-acc true-p * 1 id plus1 1 4)
(filter-acc even? * 1 id plus1 1 7)

(define (accumulate-iter combine null-val transform next a b)
  (define (iter a acc)
    (if (> a b) acc
        (iter (next a) (combine (transform a) acc))
        )
    )
  (iter a null-val)
  )


(accumulate-iter * 1 id plus1 1 4)
(accumulate-iter * 1 id plus1 1 7)
(accumulate-iter + 0 id plus1 1 5)
