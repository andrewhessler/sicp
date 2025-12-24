(define (even n) 
  (= (remainder n 2) 0)
)

(define (expmod base exp m) 
  (cond 
    ((= exp 0) 1)
    ((even? exp)
      (remainder (square (expmod base (/ exp 2) m)) m)
    )
    (else
      (remainder (* base (expmod base (- exp 1) m)) m)
    )
  )
)

(define (fermat-test n) 
  (define (try-it a)
    (= (expmod a n n) a)
  )
  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
  (cond
    ((= times 0) #t)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else #f)
  )
)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
    (report-prime (- (runtime) start-time))
    #f
  )
)

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t
)

(define (search-for-primes start find-count)
  (cond 
    ((= find-count 0) 0)
    ((= (remainder start 2) 0) (search-for-primes (+ start 1) find-count))
    (else 
      (if (timed-prime-test start) 
        (search-for-primes (+ start 2) (- find-count 1))
        (search-for-primes (+ start 2) find-count)
      )
    )
  )
)

; 10000000000000061 *** 42.919999999999995 -> 26.93
; 10000000000000069 *** 42.93000000000001 -> 26.92
; 10000000000000079 *** 43.019999999999996 -> 27.09
(search-for-primes 400 2000)

; everything is unmeasurable now... jeez louise
