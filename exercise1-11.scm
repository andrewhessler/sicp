; a function f is defined that f(n) = n if n < 3, f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3

(define (thefunction n) 
  (cond 
    ((< n 3) n)
    ((>= n 3) 
      (+ 
        (thefunction (- n 1)) 
        (* (thefunction (- n 2)) 2)
        (* (thefunction (- n 3)) 3)
      )
    )
  )
)

(define (f-iterative n)
  (define (f-loop n-1 n-2 n-3 nth)
    (if (= n nth)
        n-1  ; Final result of the computation
        (f-loop (+ n-1 (* 2 n-2) (* 3 n-3))  ; Compute f(n)
                n-1
                n-2 
                (+ 1 nth))))
  (if (< n 3)
      n
      (f-loop 2 1 0 2)))

(define (thefunctioniter n)
  (define (iter a b c count)
    (if (= count n)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1))))
  (if (< n 3)
      n
      (iter 0 1 2 2)))

(f-iterative 10)
(thefunction 10)
(thefunctioniter 10)
