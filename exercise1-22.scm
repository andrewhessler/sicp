(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond 
      ((> (square test-divisor) n) n)
      ((divides? n test-divisor) test-divisor)
      (else (find-divisor n (+ test-divisor 1)))
    )
  )
  (define (divides? dividend divisor)
    (= (remainder dividend divisor) 0)
  )
  (find-divisor n 2)
)

(define (prime? n)
  (= n (smallest-divisor n))
)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime))
)

(define (start-prime-test n start-time)
  (if (prime? n)
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

; 1009, 1013, 1019
; (search-for-primes 1000 3)

; 10007, 10009, 10037
; (search-for-primes 10000 3)

; 100003, 100019, 100043
; (search-for-primes 100000 3)

; 1000003, 1000033, 1000037
; (search-for-primes 1000000 3)

; the little ones run too fast to measure through scheme...

; (search-for-primes 100000000000 3)

; 10000000000037 *** 1.39
; 10000000000051 *** 1.38
; 10000000000099 *** 1.3599999999999999
(search-for-primes 10000000000000 3)

; 100000000000031 *** 4.32
; 100000000000067 *** 4.35
; 100000000000097 *** 4.35
; (search-for-primes 100000000000000 3)

; the little ones run too fast to measure through scheme...
; 1000000000000037 *** 13.579999999999998
; 1000000000000091 *** 13.630000000000003 
; 1000000000000159 *** 13.89
; (search-for-primes 1000000000000000 3)

; 10000000000000061 *** 42.919999999999995
; 10000000000000069 *** 42.93000000000001
; 10000000000000079 *** 43.019999999999996
; (search-for-primes 10000000000000000 3)
