(define (compose f g) (lambda (x) (f (g x))))
(define (square x) (* x x))
(define dx 0.0000001)

(define (repeated f n) 
  (define (iter g n)
    (if (= n 0) 
      g
      (iter (compose f g) (- n 1))
    )
  )
  (iter (lambda (x) x) n)
)

(define (smooth f)
  (lambda (x) 
    (/ 
      (+ 
        (f (- x dx)) 
        (f x) 
        (f (+ x dx))
      )
      3
    )
  )
)


(define (n-fold-smooth f n)
  ((repeated smooth n) f)
)

(square 5)

((smooth square) 5)
((n-fold-smooth square 15) 5)
