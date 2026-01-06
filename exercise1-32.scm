(define (acc comb null-val a b apply next)
  (if (> a b)
    null-val
    (comb (apply a) (acc comb null-val (next a) b apply next))
  )
)

(define (sum a b apply next)
  (acc + 0 a b apply next)
)

(define (product a b apply next)
  (acc * 1 a b apply next)
)

(define (inc x) (+ x 1))
(define (iden x) x)

(sum 1 5 iden inc) ; -> 15
(product 1 5 iden inc) ; -> 120

(define (acc-iter comb null-val a b apply next)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (comb result (apply a)))
    )
  )
  (iter a null-val)
)

(define (sum a b apply next)
  (acc-iter + 0 a b apply next)
)

(define (product a b apply next)
  (acc-iter * 1 a b apply next)
)

(define (inc x) (+ x 1))
(define (iden x) x)

(sum 1 5 iden inc) ; -> 15
(product 1 5 iden inc) ; -> 120
