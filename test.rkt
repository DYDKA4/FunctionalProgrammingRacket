#lang scheme/base
(define (my-force thunk) (thunk))
(define (memo-proc thunk)
  (let ((already-run? #f) (result #f))
    (lambda () (if (not already-run?)
                   (begin (set! result
                                (thunk)) (set! already-run? #t) result)
                   result))))
(define-syntax my-memo-delay
(syntax-rules ()
((_ expression)
(memo-proc (lambda () expression))
)))
(define th3m (if (or) (my-memo-delay (quotient 7 2)) (my-memo-delay (/ 1 0))))

 (let ((a 3) (b 6)) (begin ((lambda (b a) (set! a (- b a))) b a) (- b a)))
