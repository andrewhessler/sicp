(define (fast-expnt b n)
  (define (iter acc pow_max pow_count a)
    (cond 
      ((= pow_max 0) acc)
      ((> (* pow_count 2) pow_max) (iter (* acc a) (- pow_max pow_count) 1 b))
      (else (iter acc pow_max (* pow_count 2) (* a a)))
    )
  )
  (iter 1 n 1 b)
)

(define (fast-expnt-iter b n) 
  (define (iter a b n) 
    (cond
      ((= n 0) a)
      ((even? n) (iter a (* b b) (/ n 2)))
      (else (iter (* a b) b (- n 1)))
    )
  )
  (iter 1 b n)
)

(define (even? n) 
  (= (remainder n 2) 0)
)

(define (rec-fast-expnt b n)
  (cond 
    ((= n 0) 1)
    ((even? n) (square (rec-fast-expnt b (/ n 2))))
    (else (* b (rec-fast-expnt b (- n 1))))
  )
)

(define (compare b n) 
  (cond 
    ((= n 30) 0)
    (else 
      (display n)(newline)
      (display (rec-fast-expnt b n))(display " = ")(display (fast-expnt b n))(display " = ")(display (fast-expnt-iter b n))(newline)
      (compare b (+ n 1))
    )
  )
)

(compare 2 2)
