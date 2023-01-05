#lang racket
(require scheme/mpair)

(define (make-queue) (mcons 'queue '()))

(define (queue? s) ; anytype -> boolean
  (and (mpair? s) (eq? 'queue (mcar s))))

(define (empty-queue? s) ; Queuek<A> -> boolean
  (and (queue? s) (null? (mcdr s))))

(define (front-queue q)
  (if (and (queue? q) (not (empty-queue? q)))
      (mcar (mcar (mcdr q)))
      "Error"
      ))
(define (insert-queue! s e)
  (if (queue? s)
      (set-mcdr! s (mcons e (mcdr s)))
      s))

(define (delete-queue! q)
  (if (and (queue? q) (not (empty-queue? q)))
      (set-mcar! (mcdr q) (mcdr (mcar (mcdr q))))
      "Error"
      ))

(define (insert-queue! q e)
  (let ((pair (mcons e '())))
    (if (queue? q)
        (if (empty-queue? q)
            (begin 
              (set-mcar! (mcdr q) pair)
              (set-mcdr! (mcdr q) pair)
              )
            (begin
              (set-mcdr! (mcdr (mcdr q)) pair)
              (set-mcdr! (mcdr q) pair)
              )
            )
        "Error")))