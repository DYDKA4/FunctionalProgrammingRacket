#lang racket
(define (is-empty? tree)
  (equal? #() tree))
(define (is-leaf? tree)
  (if (is-empty? tree)
      #f
  (and (is-empty? (vector-ref tree 1)) (is-empty? (vector-ref tree 2)))))
(define (maxdepth tree)
  (cond 
    [(is-empty? tree) 0]
    ;[(is-leaf? tree) 0]
    [else (+ 1 (max (maxdepth (vector-ref tree 1)))
                    (maxdepth (vector-ref tree 2)))]))
(define (tree-size tree)
  (cond
   [(is-empty? tree) #t]
   [(is-leaf? tree) #t]
   [(= (abs (- (maxdepth (vector-ref tree 1)) (maxdepth (vector-ref tree 2)))) 1)
    (and (tree-size (vector-ref tree 2)) (tree-size (vector-ref tree 1)))]
   [else #f]
   ))
(define (left-leaf-lower-root left-tree right-tree root-value)
  (cond
    [(is-empty? left-tree) #t]
    [(is-empty? right-tree) #t]
    [(is-leaf? left-tree) (< (vector-ref left-tree 0) root-value)]
    [else
     (and (left-leaf-lower-root (vector-ref left-tree 1) (vector-ref left-tree 2) (vector-ref left-tree 0))
          (left-leaf-lower-root (vector-ref left-tree 1) (vector-ref left-tree 2) root-value)
          (left-leaf-lower-root (vector-ref right-tree 1) (vector-ref right-tree 2)  (vector-ref right-tree 0)))]
    )
  )
(define (right-leaf-higher-root left-tree right-tree root-value)
  (cond
    [(is-empty? left-tree) #t]
    [(is-empty? right-tree) #t]
    [(is-leaf? right-tree) (> (vector-ref left-tree 0) root-value)]
    [else
     (and (left-leaf-lower-root (vector-ref left-tree 1) (vector-ref left-tree 2) (vector-ref left-tree 0))
          (left-leaf-lower-root (vector-ref right-tree 1) (vector-ref right-tree 2) root-value)
          (left-leaf-lower-root (vector-ref right-tree 1) (vector-ref right-tree 2)  (vector-ref right-tree 0)))]
    )
  )
   
            
(define (fun2-i tree)
  (cond
    [(is-empty? tree) #t]
    [(is-leaf? tree) #t]
    [(and (tree-size tree)
          (left-leaf-lower-root (vector-ref tree 1) (vector-ref tree 2) (vector-ref tree 0))
          (right-leaf-higher-root (vector-ref tree 1) (vector-ref tree 2) (vector-ref tree 0))) #t]
    [else #f]
    )
  )
  


(define (f-2-ii lst)
  (let loop ((result 0) (lst lst) (collection '()))
    (cond
      [(eq? '() lst) result]
      [(member (car lst) collection) (loop result (cdr lst) collection)]
      [else (loop (+ result 1) (cdr lst) (cons (car lst) collection))]
      )
    )
  )
(define (stream2-3)
  (let stream2-3-gen ((curr 3) (N0 0) (N1 1) (N2 1) (N3 2) (N4 4)) ; порождающая функция
    (if (>= curr N4)
        (stream2-3-gen (+ curr 1) N1 N2 N3 N4 (+ N0 N1 N2 N3 N4))
        (stream-cons curr (stream2-3-gen (+ curr 1) N0 N1 N2 N3 N4 ))
        )
    )
  )

