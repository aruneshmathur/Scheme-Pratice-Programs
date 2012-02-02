(define (dfs graph)
   (define (dfshelper g unvisited stack path)
      (cond
	 ((null? unvisited) path)
	 ((null? stack)
	    (dfshelper g
	      	       unvisited 
		       (append (list (caar unvisited) stack)) 
		       path))
	 ((memq (car stack) path) (dfshelper g
				             unvisited 
					     (cdr stack) 
					     path))
	 (else (display (car stack)) (display "\n") (dfshelper g
		          (cdr unvisited) 
			  (append (car (neighbours (car stack) g)) (cdr stack)) 
			  (append path (list (car stack)))))))

   (define (neighbours node g)
      (cond
	 ((null? g) '())
	 ((equal? node (caar g)) (cdar g))
	 (else (neighbours node 
			   (cdr g)))))

   (dfshelper graph  graph '() '()))
