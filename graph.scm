;The Graph ADT and associated helper functions
;An example graph is:
;'((1 (2 1) (3 1)) (2 (1 1) (3 1)) (3 (1 1) (2 1)))
;
;                  3
;                  |\
;                  | \
;                  2--1
;
;The weight of every edge is 1.


;Initialize an empty graph
(define (make-graph)
  '())


;Add the vertex 'v' to the graph G
(define (add-vertex G v)
  (cond
    ((exist-vertex? G v) (display "Vertex already exists."))
    (else (cons (list v) 
		G))))


;Check if vertex exists
(define (exist-vertex? G v)
  (cond
    ((null? G) #f)
    ((eq? (caar G) v) #t)
    (else (exist-vertex? (cdr G) 
			 v))))


;Get the edge-list of vertex 'v'
(define (get-edges-list G v)
  (cond
    ((null? G) '())
    ((eq? (caar G) v) (cdar G))
    (else (get-edges (cdr G) 
		     v))))


;Set the edge-list of vertex 'v'
(define (set-edge-list! G v lst)
  (cond 
    ((null? G) (display "G is null."))
    ((eq? (caar G) v) (set-cdr! (car G) 
				lst))
    (else (set-edge-list! (cdr G) 
			  v 
			  lst))))


;Add an edge (u, v) with weight w to the graph G
(define (add-edge G u v w)
  (cond
    ((exist-vertex? u) (set-edge-list! G 
				       u 
				       (cons (list v w) 
					     (get-edge-list G u))))
    (else (begin
	    (add-vertex G u)
	    (add-edge G u v w)))))


