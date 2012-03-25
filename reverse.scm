;Reverse function

(define (rev ele)
  (cond 
    ((null? ele) '())
    (else (append (rev (cdr ele))
		  (list (car ele))))))

;Ex 2.27
;Reverse a list and it's sublists too

(define (deep-reverse lst)
  (cond 
    ((null? lst) '())
    ((pair? (car lst)) (append (deep-reverse (cdr lst))
			       (list (deep-reverse (car lst)))))
    (else (append (deep-reverse (cdr lst))
				(list (car lst))))))

