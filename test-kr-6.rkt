#lang racket/base
(require racket/stream)


; 2.II

(define (filter1 f lst)
  (reverse 
   (foldl 
    (lambda (x y) (if (f x) (cons x y) y))
    null
    lst
    )
   )
  )
(define (filter2 f lst)
  (foldr 
   (lambda (x y) (if (f x) (cons x y) y))
   null
   lst
   )
  )

; 2.III
;((λx. (λy. y x) z) y) ((λz. (z y)) (λx. x)) -> заменяем y на y1 и подставляем y в λx
;((λy1. y1 y) z) ((λz. (z y)) (λx. x)) ->  подставляем z в λy1
;z y ((λz. (z y)) (λx. x)) ->  подставляем (λx. x) в λz 
;z y ((λx. x) y) -> подставляем y в λx
;z y y Ответ

; 2.IV


(define (nthbit n)
  (if (= n 1)
      1
      (let loop ((x n) (pow 4))
        (cond
          [(< x pow) 0]
          [(= x pow) 1]
          [(> x pow) (loop x (* pow 4))]))
      )
  )

; 2.VI

(define (nonfactorials)
  (let stream2-3-gen ((curr 3) (factorial 2) (multiplyer 3)) ; порождающая функция
    (cond
        [(< curr factorial)(stream-cons curr (stream2-3-gen (+ curr 1) factorial multiplyer))]
        [(= curr factorial)(stream2-3-gen (+ curr 1) (* factorial multiplyer) (+ multiplyer 1))]
        [(> curr factorial)(stream2-3-gen curr (* factorial multiplyer) (+ multiplyer 1))]
        )
    )
  )


