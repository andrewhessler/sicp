(define (compose f g) (lambda (x) (f (g x))))
(define (square x) (* x x))

(define (repeated f n) 
  (define (iter g n)
    (if (= n 0) 
      g
      (iter (compose f g) (- n 1))
    )
  )
  (iter (lambda (x) x) n)
)

((repeated square 2) 5)
