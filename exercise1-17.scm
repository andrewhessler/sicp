(define (even? x) 
  (= (remainder x 2) 0)
)

(define (h x) 
  (/ x 2)
)

(define (d x) 
  (* x 2)
)

(define (fast-m a b)
  (display "a: ")(display a)(display " b: ")(display b)(newline)
  (cond 
    ((= b 1) a)
    ((even? b) (fast-m (d a) (h b)))
    (else (fast-m (+ a a) (- b 1)))
  )
)

(fast-m 7 10)

(define (rec-fast-expnt b n)
  (cond 
    ((= n 0) 1)
    ((even? n) (square (rec-fast-expnt b (/ n 2))))
    (else (* b (rec-fast-expnt b (- n 1))))
  )
)

(define (rec-fast-m a b)
  (display "a: ")(display a)(display " b: ")(display b)(newline)
  (cond
    ((= b 1) a)
    ((even? b) (d (rec-fast-m a (h b))))
    (else (+ a (rec-fast-m a (- b 1))))
  )
)

(rec-fast-m 10 8)
(rec-fast-m 8 4)
(rec-fast-m 8 7)
