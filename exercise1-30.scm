(define (sum a b apply next)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (apply a)))
    )
  )
  (iter a 0)
)

(define (iden x) x)
(define (inc x) (+ x 1))
(sum 1 5 iden inc)
