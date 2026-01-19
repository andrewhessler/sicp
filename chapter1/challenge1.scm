 (define (count-change amount)
	(cc amount 5))

 (define (cc amount kinds-of-coins)
	(cond ((= amount 0) 1)
	 ((or (< amount 0) (= kinds-of-coins 0)) 0)
	 (else (+ (cc (- amount
			 (first-denomination kinds-of-coins))
			kinds-of-coins)
		  (cc amount
			(- kinds-of-coins 1))))))

 (define (first-denomination kinds-of-coins)
	(cond ((= kinds-of-coins 1) 1)
	 ((= kinds-of-coins 2) 5)
	 ((= kinds-of-coins 3) 10)
	 ((= kinds-of-coins 4) 25)
	 ((= kinds-of-coins 5) 50)))

; lets assume always 5 kinds of counts right now
(define (count-change-2 amount)
  (define (cc-fifties amount acc) 
    (cond 
      ((= amount 0) (+ acc 1))
      ((< amount 0) acc)
      (else (cc-fifties 
              (- amount 50) 
              (cc-quarters amount acc)))    
          ))
  (define (cc-quarters amount acc) 
    (cond 
      ((= amount 0) (+ acc 1))
      ((< amount 0) acc)
      (else (cc-quarters 
              (- amount 25) 
              (cc-dimes amount acc)))    
          ))
  (define (cc-dimes amount acc) 
    (cond 
      ((= amount 0) (+ acc 1))
      ((< amount 0) acc)
      (else (cc-dimes 
              (- amount 10) 
              (cc-nickels amount acc)))    
          ))
  (define (cc-nickels amount acc) 
    (cond 
      ((= amount 0) (+ acc 1))
      ((< amount 0) acc)
      (else (cc-nickels 
              (- amount 5) 
              (cc-pennies amount acc)))    
          ))
  (define (cc-pennies amount acc) 
    (cond 
      ((= amount 0) (+ acc 1))
      ((< amount 0) acc)
      (else (cc-pennies 
              (- amount 1) 
              acc)) 
          ))
  (cc-fifties amount 0)
)

(count-change 200)
(count-change-2 200)
