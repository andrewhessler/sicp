(define (even? x) 
  (= (remainder x 2) 0)
)

(define (h x) 
  (/ x 2)
)

(define (d x) 
  (* x 2)
)

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

(define (iter-fast-expnt b n) 
  (define (iter a b n) 
    (cond
      ((= n 0) a)
      ((even? n) (iter a (* b b) (/ n 2)))
      (else (iter (* a b) b (- n 1)))
    )
  )
  (iter 1 b n)
)

(define (iter-fast-m a b)
  (define (iter a b s)
    (display "a: ")(display a)(display " b: ")(display b)(display " s: ")(display s)(newline)
    (cond 
      ((= b 1) (+ a s)) 
      ((even? b) (iter (d a) (h b) s))
      (else (iter a (- b 1) (+ s a)))
    )
  )
  (iter a b 0)
)

(rec-fast-m 7 7)
(iter-fast-m 7 7)
