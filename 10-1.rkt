#lang racket/base

(define Y
    (lambda (f) (
        (lambda (x) (x x))
        (lambda (g) (f (lambda args (apply (g g) args)))))
    )
)

(define n!!!! (lambda (n)
    ((Y (lambda (f) (lambda (i result) (
        cond
            ((< i 2) result)
            ((= i 2) (* result 2))
            ((= i 3) (* result 3))
            (else
                (f (- i 3) (* i result))
            )
        ))))
        n 1)))

