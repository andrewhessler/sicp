(define tolerance 0.0000001)
(define (compose f g) (lambda (x) (f (g x))))
(define (repeated f n) 
  (define (iter g n)
    (if (= n 0) 
      g
      (iter (compose f g) (- n 1))
    )
  )
  (iter (lambda (x) x) n)
)

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

(define (average x y) 
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x)))
)

(define (log-2 x)
  (/ (log x) (log 2))
)

; for x = 2, x = 3, ...
; root damp-count
; 1  0
; 2  1
; 3  1
; 4  2
; 5  2
; 6  2
; 7  2
; 8  3
; 15 3
; 16 4
; 31 4
; 32 5
; floor of log-2

(define (nth-root x n)
  (define avg-damps (floor (log-2 n)))
  (fixed-point
    ((repeated average-damp avg-damps)
      (lambda (y) (/ x (expt y (- n 1))))
    )
    1.0
  )
)

(nth-root 32 5)

