(define (pascal-me row col)
  (cond 
        ((or (= col 1) (= col row) (= row 1)) 1)
        (else (+ (pascal (- row 1)  (- col 1))
                 (pascal (- row 1) col)))))

(define (pascal row col)
  (cond ((= row 1) 1)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal (- row 1)  (- col 1))
                 (pascal (- row 1) col)))))
(pascal 3 2)
(pascal-me 3 2)
