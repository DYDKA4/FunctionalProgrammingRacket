#lang racket
(define (taskI lst)
  (define (minim lst)
    (cond 
      [(null? (cdr lst)) (car lst)]
      [(< (car lst) (minim (cdr lst))) (car lst)]
      [else (minim (cdr lst))]))
  (define (bild-lst new-lst lst min-val i)
    (cond
      [(null? (cdr lst))
       (if (= (car lst) min-val) (cons i new-lst) new-lst)]
      [(= (car lst) min-val) (bild-lst (cons i new-lst) (cdr lst) min-val (+ i 1))]
      [else (bild-lst new-lst (cdr lst) min-val (+ i 1))]
      )
    )
  (cond
    [(equal? lst '()) '()]
    [else (bild-lst '() lst (minim lst) 0)]
    )
  )
(define (taskII t s)
  (let sub-function ((deep 4) (t (vector->list t)) (total 0) (s s))
    (cond
      [(null? t) total]
      [(vector?(car t)) (sub-function deep (cdr t) (sub-function (* deep 4) (vector->list (car t)) total s) s)]
      [(= (car t) 1) (sub-function deep (cdr t) (+ total (/ s deep)) s)]
      [(null? (cdr t)) total]
      [else
       (sub-function deep (cdr t) total s)]
      )
    )
  )
(define (taskIII t)
    (=(car(call/cc
     (lambda (cc-exit)
       (let sub-function ((deep 4) (t (vector->list t)) (result '(0 0)))
         (cond
           [( >= (car result) 0.5) (cc-exit '(1 0))]
           [( > (car ( cdr result)) 0.5)(cc-exit '(0 1))]
           [(null? t) result ]
           [(vector?(car t))(sub-function deep (cdr t) (sub-function (* deep 4) (vector->list (car t)) result))]
           [(= (car t) 1) (sub-function deep (cdr t) (list (car result) (+ (car ( cdr result)) (/ 1 deep))))]
           [(null? (cdr t)) result]
           [else
            (sub-function deep (cdr t) (list (+ (car result) (/ 1 deep)) (car ( cdr result))))]
           )
         )
       )
     )
          )
      0)
  )

(define (taskIV-cc t s cc)
  (cond
    [(and (vector? t) (= (vector-length t) 1) (= (vector-ref t 0) 0)) (cc 0)]
    [(and (vector? t) (= (vector-length t) 1) (= (vector-ref t 0) 1)) (cc s)]
    [(equal? 1 t) (cc s)]
    [(equal? 0 t) (cc 0)]
    [else
     (taskIV-cc (vector-ref t 0)(/ s 4) (lambda(x)(taskIV-cc (vector-ref t 1)(/ s 4)
                                                        (lambda(y)(taskIV-cc (vector-ref t 2)(/ s 4)
                                                                          (lambda(z)(taskIV-cc (vector-ref t 3)(/ s 4)
                                                                                            (lambda(w)(cc (+ x y z w))))))))))]
    )
  )

(define (taskV . args)
    (foldl (lambda(x y)(lambda(z)(x (y z)))) (car args) (cdr args))  
)
    
