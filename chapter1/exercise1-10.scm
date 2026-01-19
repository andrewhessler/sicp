(define (A x y) 
  (cond 
    ((= y 0) 0)
    ((= x 0) (* 2 y))
    ((= y 1) 2)
    (else (A (- x 1) (A x (- y 1))))
))

(define (f n) (A 2 n))

(f 0)
(f 1)
(f 2)
(f 3)
(f 4)

