(define (mergesort lst)
  (define (merge l1 l2)
    (cond
      ((and (null? l1) (null? l2)) '())
      ((null? l1) l2)
      ((null? l2) l1)
      ((<= (car l1) (car l2)) (cons (car l1) 
				   (merge (cdr l1) 
					  l2)))
      ((> (car l1) (car l2)) (cons (car l2)
				   (merge l1 
					  (cdr l2))))))
  (define (splitsort lst)
    (let ((index (round (/ (length lst)
			   2))))
      (cond
	((null? lst) ())
        ((equal? 1 (length lst)) lst)
        (else (merge (splitsort (take lst 
				      index)) 
		   (splitsort (drop lst 
				    (- (length lst) 
				       index))))))))
  (splitsort lst))

