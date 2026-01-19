; phi = (1 + sqrt(5)) / 2 ~= 1.6180
; f(x) = x
; f(x) = 1 + (1 / x)
; f(phi) = 1 + (1 / phi)
; 1 + (1 / phi) ~= 1.6180
(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next)
      ) 
    )
  )
  (try first-guess)
)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
