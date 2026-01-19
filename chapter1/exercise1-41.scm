(define (inc x) (+ x 1))
(define (double g) (lambda (x) (g (g x))))

(((double (double double)) inc) 5) ; 21
