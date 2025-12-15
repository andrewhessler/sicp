(define (get-smallest a b c) (cond ((and (< a b) (< a c) a))
                                   ((and (< b a) (< b c) b))
                                    (else c)
                                    ))))

(define (get-largest a b c) (cond ((and (> a b) (> a c) a))
                                   ((and (> b a) (> b c) b))
                                    (else c)
                                    ))))

(define (get-middle a b c) (cond ((and (not (= (get-smallest a b c) a)) (not (= (get-largest a b c) a))) a)
                                 ((and (not (= (get-smallest a b c) b)) (not (= (get-largest a b c) b))) b)
                                 (else c)
                                 ))

(define (square a) (* a a))

(define (sum-of-two-squares a b) (+ (square a) (square b)))

(define (sum-of-two-squares-largest-of-three a b c) (sum-of-two-squares (get-largest a b c) (get-middle a b c)))

(sum-of-two-squares-largest-of-three 3 4 5)
