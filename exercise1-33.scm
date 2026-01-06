(define (acc-fil filter comb null-val a b apply next)
  (cond ((> a b) null-val)
        ((filter a) (comb (apply a) (acc-fil filter comb null-val (next a) b apply next)))
        (else (comb (acc-fil filter comb null-val (next a) b apply next)))
  )
)

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

(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (iden x) x)

(define (sum-of-sq-pr a b)
  (acc-fil prime? + 0 a b square inc)
)

(sum-of-sq-pr 1 7) ; -> 1,2,3,5,7 -> 1 + 4 + 9 + 25 + 49 -> 88

(define (product-of-relative-primes n)
  (define (gcd-1? x)
    (= (gcd x n) 1)
  )
  (acc-fil gcd-1? * 1 2 n iden inc)
)

(product-of-relative-primes 5) ; -> 1 * 2 * 3 * 4 -> 24 ; makes sense, it's prime
(product-of-relative-primes 8) ; -> 3 * 5 * 7 -> 105

