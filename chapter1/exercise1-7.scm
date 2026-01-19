(define (square x) (* x x))

(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.0000001))

(define (average x y) 
  (/ (+ x y) 2))

(define (improve guess x) 
  (average guess (/ x guess)))

(define (sqrt-iter guess x) 
  (if (good-enough? guess (improve guess x)) guess 
    (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))


(sqrt 25)
