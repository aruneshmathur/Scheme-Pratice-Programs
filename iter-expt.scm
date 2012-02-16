; An iterative version of fast-expt
; Exercise number 1.16 in SICP

(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
		((even? n) (iter a (square b) (/ n 2)))
		(else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
