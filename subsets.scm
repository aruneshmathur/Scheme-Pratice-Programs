(define (subsets lst)
    (if (null? lst)
      (list '())
      (let ((rest (subsets (cdr lst))))
	(append rest (map (lambda (x) (append (list (car lst)) x))
			  rest)))))
    
