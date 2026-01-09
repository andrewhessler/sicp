; a
(define (inc x) (+ x 1))
(define (cont-frac n d k)
  (define (inner i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (inner (inc i))))
    )
  )
  (inner 1)
)

; .6179
(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           10
)

; .6180... so 11 k needed for 4 decimal accuracy lol
(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0)
           11
)

;b
(define (dec x) (- x 1))

(define (cont-frac-i n d k)
  (define (iter i result)
    (if (= i 0)
      result
      (iter (dec i) (/ (n i) (+ result (d i))))
    )
  )
  (iter k 0)
)

(cont-frac-i (lambda (x) 1.0)
           (lambda (x) 1.0)
           11
)
