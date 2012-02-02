(define (bfs graph)
   (define (bfshelper g unvisited queue path)
      (cond
	 ((null? unvisited) path)
	 ((null? queue)
	    (bfshelper g
	      	       unvisited 
		       (append queue (list (caar unvisited))) 
		       path))
	 ((memq (car queue) path) (bfshelper g
				             unvisited 
					     (cdr queue) 
					     path))
	 (else (bfshelper g
		          (cdr unvisited) 
			  (append (cdr queue) (car (neighbours (car queue) g))) 
			  (append path (list (car queue)))))))

   (define (neighbours node g)
      (cond
	 ((null? g) '())
	 ((equal? node (caar g)) (cdar g))
	 (else (neighbours node 
			   (cdr g)))))

   (bfshelper graph  graph '() '()))
