; A logarithmic and linear  version of multiplication
; Iterative version of log-mult
; Exercise number 1.17, 1.18 in SICP

(define (linear-mult a b)
  (if (= b 0)
    0
    (+ a (linear-mult a 
		      (- b 1)))))

(define (log-mult a b)
  (cond ((= a 0) 0)
	((even? a) (log-mult (half a) 
			     (double b)))
	(else (+ b (log-mult (- a 1) 
			     b)))))

(define (half a)
  (/ a 2))

(define (double a)
  (* a 2))

(define (iter-log-mult a b)
  (define (mult c a b)
    (cond
      ((= a 0) c)
      ((even? a) (mult c 
		       (half a) 
		       (double b)))
      (else (mult (+ c b) (- a 1) b))))

  (mult 0 a b))
   
