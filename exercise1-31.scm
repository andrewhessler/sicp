; a
(define (product a b apply next)
  (if (> a b)
    1
    (* (apply a) (product (next a) b apply next))
  )
)

(define (iden x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(product 1 5 iden inc)

(define (factorial n)
  (product 2 n iden inc)
)

(define (pi-approx n)
  (define (top x)
    (if (= x n)
      (* 2.0 (+ n 1)) 
      (square (* 2.0 (+ x 1)))
    )
  )
  (define (bot x)
    (square (+ (* 2.0 x) 1))
  )
  (* 8 (/ 
        (product 1 n top inc)
        (product 1 n bot inc)
       )
  )
)

(pi-approx 40)

(define (pi-approx-formula n)
  (define (term x)
    (/
      (* (* 2.0 x) (* 2.0 (+ x 1))) 
      (square (+ (* 2.0 x) 1))
    )
  )
  (* 4 (product 1 n term inc))
)

(pi-approx-formula 150000)


; b
(define (product-iter a b apply next)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (apply a)))
    )
  )
  (iter a 1)
)

(define (pi-approx-formula-iter n)
  (define (term x)
    (/
      (* (* 2.0 x) (* 2.0 (+ x 1))) 
      (square (+ (* 2.0 x) 1))
    )
  )
  (* 4 (product-iter 1 n term inc))
)

(pi-approx-formula-iter 1000000000)
