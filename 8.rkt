#lang racket
(require math/number-theory)


;(stream-ref (stream-filter prime? (in-range 2 100000000 1)) 2)
;(define (stream-scale s f)
 ; (stream-map (lambda (x) (* x f)) s)

(define (powers)
  (let pow-gen ((a 1) (f2 5)) ; порождающая функция
    (if (> a f2)
        (stream-cons f2 (pow-gen a (* f2 5)))
        (stream-cons a (pow-gen (* a 2) f2)))))

(define (print-powers-in-range n)
  (let loop ((j 0))
    (if (= j n)
        "end"
        (begin
          (println (stream-ref (powers) j))
          (loop (+ j 1))
          )
        )
    )
  )

(print-powers-in-range 10)
