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


; SICP 2.35

(define (count-leaves t)
  (accumalator + 0 (map (lambda (node)
			  (if (pair? node)
			    (count-leaves node)
			    1))
			t)))

; SICP 2.36

(define (accumalate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumalator op 
		       init 
		       (map car seqs))
	  (accumalate-n op init (map cdr seqs)))))

; SICP 2.37

(define (dot-product u v)
  (accumalator + 0 (map * u v)))


(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))


(define (transpose mat)
  (accumalate-n cons '() mat))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))
	 m)))
