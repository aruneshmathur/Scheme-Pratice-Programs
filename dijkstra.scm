;Get the weight of edge (uv)

(define (edge-wt G u v)
  (get-vertex (get-neighbours-list G u) 
	      v))


;Get the weight of edge (u,v) where the 
;adjacency list of u is lst

(define (get-vertex lst v)
  (cond
    ((null? lst) #f)
    ((eq? (caar lst) v) (cadar lst))
    (else (get-vertex (cdr lst) v))))
	

;Get the neighbours of vertex 'v' from 
; a given weighted graph G

(define (get-neighbours G v)
  (pick-odd (flatten (get-neighbours-list G v))))


;Get the list of neighbours and their weights 
;of vertex v

(define (get-neighbours-list G v)
  (cond
    ((null? G) #f)
    ((eq? (caar G) v) (cdar G))
    (else (get-neighbours-list (cdr G)
			       v))))


;Flatten given object

(define (flatten lst)
  (cond
    ((null? lst) '())
    ((or (string? lst) (number? lst)) (list lst))
    (else (append (flatten (car lst)) 
		  (flatten (cdr lst))))))


;Get elements at odd positions

(define (pick-odd lst)
  (pick lst 
	(length lst)))

(define (pick lst n)
  (cond 
    ((= n 0) '())
    ((eq? (remainder n 2) 0) (cons (car lst)
				   (pick (cdr lst) (- n 1))))
    (else (pick (cdr lst) 
		(- n 1)))))


;Get the vertices from a graph G

(define (get-vertices G)
  (cond 
    ((null? G) '())
    (else (cons (caar G) 
		(get-vertices (cdr G))))))


;Create the vertex list for Graph G

(define (make-vertex-list G)
  (get-vertices G))


;Get the current shortest distance to vertex 'v'

(define (get-vertex-dist D v)
  (cond
    ((null? D) #f)
    ((eq? (caar D) v) (cdar D))
    (else (get-vertex-dist (cdr D) 
			   v))))


;Set the distance to vertex 'v' to val

(define (set-vertex-dist D v val)
  (cond
    ((null? D) #f)
    ((eq? (caar D) v) (set-car! D 
				(list v val)))
    (else (set-vertex-dist (cdr D)
			   v
			   val))))


;Relax vertex 'v' from vertex 'u'
;'w' is the weight of the edge (u, v)

(define (relax D u v w)
  (set-vertex-dist D 
		   v 
		   (+ (get-vertex-dist D u)
		      w)))



