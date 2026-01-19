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

(define (euler-d i)
  (if (= (remainder i 3) 2)
    (* 2(/ (+ i 1) 3))
    1
  )
)

(cont-frac-i (lambda (x) 1.0) euler-d 9) ; .71828

; euler - 2 = .71828

