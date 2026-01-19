(define tolerance 0.0000001)
(define (iterative-improve check improve)
  (lambda (x)
    (define (try guess)
      (let ((next (improve guess)))
      (display next)(display ":")(display guess)(newline)
        (if (check guess next)
          next
          (try next)
        )
      ) 
    )    
    (try x)
  )
)


(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  ((iterative-improve close-enough? f) first-guess)
)


(define (average x y) 
  (/ (+ x y) 2.0))

(define (sqrt x)
  (define (close-enough? guess next)
    (< (abs (- (square guess) x)) tolerance)
  )
  ((iterative-improve close-enough? (lambda (y) (average y (/ x y)))) x)
)

(sqrt 25)
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
