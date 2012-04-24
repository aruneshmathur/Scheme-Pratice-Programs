(load "accumalator.scm")

(define (enumerate-interval start end)
  (if (> start end)
    '()
    (cons start 
	  (enumerate-interval (1+ start) end))))

;(accumalator append 
;	     '()
;	     (map (lambda (i)
;		    (map (lambda (j) (list i j))
;			 (enumerate-interval 1 (- i 1))))
;		  (enumerate-interval 1 10)))

(define (equal? lst1 lst2)
  (cond
    ((and (not (pair? lst1)) 
	  (not (pair? lst2)))
     (eq? lst1 lst2))

     ((and (pair? lst1) (pair? lst2))
      (and (equal? (car lst1) (car lst2))
	   (equal? (cdr lst1) (cdr lst2))))

     (else #f)))
