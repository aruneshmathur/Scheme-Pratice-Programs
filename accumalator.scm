(define accumalator (lambda (op initial sequence)
		      (if (null? sequence)
			initial
			(op (car sequence)
			    (accumalator op 
					 initial 
					 (cdr sequence))))))


(define append-acc (lambda (seq1 seq2)
		     (accumalator cons 
				  seq2 
				  seq1)))

(define length-acc (lambda (sequence)
		      (accumalator (lambda (x y) (1+ y)) 
				   0 
				   sequence)))

(define map-acc (lambda (p sequence)
		  (accumalator (lambda (x y) (cons (p x)
						    y)) 
			       '() 
			       sequence)))

(define (horner-eval x coefficient-sequence)
  (accumalator (lambda (this-coeff higher-terms)
		(+ (* 2 higher-terms) this-coeff))
	       0
	       coefficient-sequence))
