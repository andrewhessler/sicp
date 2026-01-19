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
(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
      x
      (- (square x))
    )
  )
  (define (d i)
    (- (* 2.0 i) 1)
  )
  (cont-frac-i n d k)
)
(tan-cf 0.785398 9) ; hoping for ~= 1
; got .9999996732051568, pretty good
