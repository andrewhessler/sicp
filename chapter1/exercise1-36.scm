(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance)
  )
  (define (try guess iter)
    (let ((next (f guess)))
      (display "guess ")(display iter)(display": ")(display next)(newline)
      (if (close-enough? guess next)
        next
        (try next (1+ iter))
      ) 
    )
  )
  (newline)(display "guess 0: ")(display first-guess)(newline)
  (try first-guess 1)
)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (lambda (x) 
    (* 0.5 (+ x (/ (log 1000) (log x))))
  ) 2.0)

; takes about a 1/4th of the steps with average damping

(fixed-point (lambda (x) (/ (log 1000) (log x))) 300.0)
(fixed-point (lambda (x) 
    (* 0.5 (+ x (/ (log 1000) (log x))))
  ) 300.0)


