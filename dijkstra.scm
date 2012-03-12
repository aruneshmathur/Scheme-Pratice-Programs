(define (quicksort l)
  (if (null? l)
    '()
    (append (quicksort (filter (lambda (x) (> (cadar l) (cadr x))) (cdr l)))
	    (list (car l))
	    (quicksort (remove (lambda (x) (> (cadar l) (cadr x))) (cdr l))))))


;Get the weight of edge (uv)
(define (edge-wt G u v)
  (define (vertex-wt lst v)
    (cond
      ((null? lst) #f)
      ((eq? (caar lst) v) (cadar lst))
      (else (vertex (cdr lst) v))))
    (vertex-wt (adj-list G u) 
	      v))


;Get the neighbours of vertex 'v'
(define (adj-vertices G v)
  (define (adj lst)
    (cond
      ((null? lst) '())
      (else (cons (caar lst) 
		  (adj (cdr lst))))))
  (adj (adj-list G v)))
					  

;Get the list of neighbours and their weights 
;of vertex v
(define (adj-list G v)
  (cond
    ((null? G) #f)
    ((eq? (caar G) v) (cdar G))
    (else (adj-list (cdr G)
		    v))))


;Get the list vertices 
(define (list-vertices G)
  (cond 
    ((null? G) '())
    (else (cons (caar G) 
		(list-vertices (cdr G))))))


;Create the vertex list for Graph G
(define (make-vertex-list G)
  (list-vertices G))


;Get the current shortest distance to vertex 'v'
(define (get-vertex-dist D v)
  (cond
    ((null? D) #f)
    ((eq? (caar D) v) (cdar D))
    (else (get-vertex-dist (cdr D) 
			   v))))


;Set the distance to vertex 'v' to val

(define (set-vertex-dist! D v val)
  (cond
    ((null? D) #f)
    ((eq? (caar D) v) (set-car! D 
				(list v val)))
    (else (set-vertex-dist! (cdr D)
			   v
			   val))))


;Relax vertex 'v' from vertex 'u'
;'w' is the weight of the edge (u, v)

(define (relax D u v w)
  (set-vertex-dist! D 
		   v 
		   (+ (get-vertex-dist D u)
		      w)))


;Create the distance list for the vertices
(define (make-dist-list vertex-list)
  (cond
    ((null? vertex-list) '())
    (else (cons (list (car vertex-list) 99999)
		(make-dist-list (cdr vertex-list))))))


;Dijkstra's algorithm
(define (dijkstra G start end)
  (define (csp current D)
    (define (for-adj v w)
      (if (and (< (+ (get-vertex-dist D current) 
		w) 
		  (get-vertex-dist v))
	       (memq v D))
	(relax D current v w)))

    (define (iter-vert lst)
      (cond
	((null? lst) '())
	(else (for-adj (car lst) (edge-wt G 
					  current 
					  (car lst))))))
    (cond
      ((null? D) '())
      (else (iter-vert (adj-vertices current)))))
   
  (define (spa current D)
    (cond 
      ((null? D) (display "No shortest path found."))
      ((eq? current end) (get-vertex-dist D end))
      (else (let* ((Di (quicksort D))
		  (l (caar Di)))
	      (begin
		(csp l (cdr Di))
		(spa l (cdr Di)))))))
  (spa start (make-dist-list (make-vertex-list G)))) 

