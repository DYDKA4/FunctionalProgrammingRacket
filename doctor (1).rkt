; заготовка "Доктора". Январь 2022
#lang scheme/base
(require racket/list)
; В учебных целях используется базовая версия Scheme

; основная функция, запускающая "Доктора"
; параметр name -- имя пациента

(define (ask-patient-name)
 (begin
  (println '(next!))
  (println '(who are you?))
  (print '**)
  (car (read))
 ) 
)

(define (visit-doctor name)
  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop-v1 name '())
)

(define (visit-doctor-v1 stop-word limit)
  (let loop ((name (ask-patient-name))(i 0))
    (begin
      (if (or (equal? name stop-word) ( = limit i))
          (println '(time to go home))
          (begin
            (printf "Hello, ~a!\n" name)
            (print '(what seems to be the trouble?))
            (doctor-driver-loop-v2 name '())
            )
          )
      (if (= limit (+ i 1)) (println '(time to go home))
          (loop (ask-patient-name) (+ i 1)))
      )
    )
  )

; цикл диалога Доктора с пациентом
; параметр name -- имя пациента
(define (doctor-driver-loop name)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (print '(see you next week)))
            (else (print (reply user-response)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop name)
             )
       )
      )
)

(define reply-strategies
  (list
   (list 2
         (lambda(x y) #t)
         (lambda(x y)(hedge-answer)))
   (list 5
         (lambda(x y) #t)
         (lambda(x y)(qualifier-answer x)))

   (list 9
         (lambda(x y) (not(eq? y '())))
         (lambda(x y)(history-answer-v1 y)))
   (list 100
         (lambda(x y) (if (is-context-word-in? x) #t #f))
         (lambda(x y)(context-answer-v1 x))) 
  )
)

(define (doctor-driver-loop-v1 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (println '(see you next week)))
            (else (print (reply-v2 user-response history)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v1 name (if (member user-response history) history (cons (list user-response) history))) ; проверить 
             )
       )
      )
)
(define (doctor-driver-loop-v2 name history)
    (newline)
    (print '**) ; доктор ждёт ввода реплики пациента, приглашением к которому является **
    (let ((user-response (read)))
      (cond 
	    ((equal? user-response '(goodbye)) ; реплика '(goodbye) служит для выхода из цикла
             (printf "Goodbye, ~a!\n" name)
             (println '(see you next week)))
            (else (print (reply-v3 user-response history reply-strategies)) ; иначе Доктор генерирует ответ, печатает его и продолжает цикл
                  (doctor-driver-loop-v2 name (if (member user-response history) history (cons (list user-response) history))) ; проверить 
             )
       )
      )
)
; генерация ответной реплики по user-response -- реплике от пользователя
(define (reply user-response)
      (case (random 2) ; с равной вероятностью выбирается один из двух способов построения ответа
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge-answer))  ; 2й способ
      )
)

(define (reply-v1 user-response history)
      (case (random (if (eq? history '()) 2 3)) ; с равной вероятностью выбирается один из двух способов построения ответа ; потестить
          ((0) (qualifier-answer user-response)) ; 1й способ
          ((1) (hedge-answer))  ; 2й способ
          ((2) (history-answer user-response history))
      )
)

(define (reply-v2 user-response history)
      (case (random (if (eq? history '()) 1 0) (if (is-context-word-in? user-response) 4 3)) ; с равной вероятностью выбирается один из двух способов построения ответа ; тестить
          ((1) (qualifier-answer user-response)) ; 1й способ
          ((2) (hedge-answer))  ; 2й способ
          ((0) (history-answer-v1 history))
          ((3) (context-answer-v1 user-response))
      )
)


(define (reply-v3 user-response history reply-strategies)
;  (let *(
;         (strategy-list (filter (lambda (x)((cadr x) user-response history)) reply-strategies))
;         (weight (foldl (lambda(x y)(+ (car x) y)) 0 strategy-list)))
;    weight
;    )
;  )
  (let ((strategy-list (filter (lambda (x)((cadr x) user-response history)) reply-strategies)))
    (let ((weight (foldl (lambda(x y)(+ (car x) y)) 0 strategy-list)))
      (let loop ((rand (random weight)) (p (caar strategy-list)) (current-strategy (car strategy-list)) (other-strategies (cdr strategy-list)))
        (call/cc 
         (lambda (cc-exit)
           (if (<= rand p)
                (cc-exit ((caddr current-strategy) user-response history))
               (loop (- rand p) (caar other-strategies) (car other-strategies) (cdr other-strategies))
               )
           )
        )
      )
    )
  )
;(define (reply-v3)
(define context-data '( 
  ( ; начало данных 1й группы
    (depressed suicide exams university) ; список ключевых слов 1й группы
    ( ; список шаблонов для составления ответных реплик 1й группы 
	  (when you feel depressed, go out for ice cream)
          (depression is a disease that can be treated)
          (if needed I can refer you to a psychiatrist)
          (keep attending our sessions and we will beat this)
	)
  ) ; завершение данных 1й группы
  ( ; начало данных 2й группы ...
    (mother father parents brother sister uncle aunt grandma grandpa)
    (
	  (tell me more about your * , i want to know all about your *)
          (why do you feel that way about your * ?)
          (what can you tell about your *)
          (how do you feel about your *)
	)
  )
  (
    (university scheme lections)
	(
	  (your education is important)
	  (how much time do you spend to education ?)
          (is * the hardest one ? )
          (are very important nowadays)
	)
  )
  (
    (scheme lisp scala)
	(
	  (functional programming is a most attractive programming paradigm isnt it ?)
	  (* rules whole world !)
          (* is the hardest !)
          (functional programming is very exciting)
	)
  )
  (
    (c++ java c#)
	(
	  (object-oriented programming is a prettiest programming paradigm ever isnt it ?)
	  (* is the best one !)
          (* is the easiest !)
          (object-oriented is very important nowadays) 
	)
  )
  (
   (good well nice)
   (
    (i see you are in a good mood)
    (why is it so * ?)
    (this is a good approach)
    (go on about it)
    )
   )
  )
  )
  
(define (context-response user-response)
  (list  user-response (filter-not null?
              (map (lambda (element)
                     ;(println (car element))
                     (let loop ((response-elem user-response) (key-words (car element)))
              (cond
                [(null? response-elem ) '()]
                [(member response-elem key-words) (cadr element)]
                [else
                 (loop '() key-words)]
                )
              ))context-data)
              )
         ))

(define (is-context-word-in? user-response)
  (let loop
      ((key-words (let remove-lst-in-lst ((words (foldl (lambda (v l) (cons (car v) l)) '() context-data)) (result '()))
         (cond
           [(null? words) result]
           [(list? words) (remove-lst-in-lst (cdr words) (remove-lst-in-lst (car words) result))]
           [(member words result) result]
           [else (cons words result)]
           )
         ))
       (response user-response)
       )
    (cond
      [(eq? response '()) #f]
      [(not (list? response)) (if (member response key-words) #t #f)]
      [(member (car response) key-words) #t]
      [else
       (loop (cdr response) key-words)]
      )
    )
  )
    
         
(define (context-answer user-response history)
  (cond
    [is-context-word-in? user-response
                         (define (replace-word word part-phrase phrase result)
                           (cond
                             [ (null? (cdr phrase)) (if (equal? part-phrase '*) (cons word result) (cons part-phrase result))]
                             [(equal? part-phrase '*) (replace-word word (cadr phrase) (cdr phrase) (cons word result))]
                             [else
                              (replace-word word (cadr phrase) (cdr phrase) (cons part-phrase result))]))
                         (reverse(let ((result (let ((phrase (let ((phrases (context-response(let ((filtered-lst (filter is-context-word-in? user-response)))
                                                                                              (list-ref filtered-lst (random (length filtered-lst)))))))
                                         (list (car phrases) (list-ref (cadr phrases) (random (length (cadr phrases))))))))
                           (list (car phrase) (list-ref (cadr phrase) (random (length (cadr phrase))))))))
                           (replace-word (car result) (car (cadr result)) (cadr result) '())))
                           
                         ]
    [else (reply-v2 user-response history)]
    )
  )

(define (context-answer-v1 user-response)
  (define (replace-word word part-phrase phrase result)
    (cond
      [ (null? (cdr phrase)) (if (equal? part-phrase '*) (cons word result) (cons part-phrase result))]
      [(equal? part-phrase '*) (replace-word word (cadr phrase) (cdr phrase) (cons word result))]
      [else
       (replace-word word (cadr phrase) (cdr phrase) (cons part-phrase result))]
      )
    )
  (reverse(let ((result (let ((phrase (let ((phrases (context-response(let ((filtered-lst (filter is-context-word-in? user-response)))
                                                                        (list-ref filtered-lst (random (length filtered-lst)))))))
                                        (list (car phrases) (list-ref (cadr phrases) (random (length (cadr phrases))))))))
                          (list (car phrase) (list-ref (cadr phrase) (random (length (cadr phrase))))))))
            (replace-word (car result) (car (cadr result)) (cadr result) '())
            ))
  )

(define (history-answer user-response history)
  (cond
    [(eq? history '()) (reply-v1 user-response history)]
    [else
     (let ((is-in (member user-response history)))
       (if is-in (append '(earlier you said that) user-response) (reply-v1 user-response history)))]
    )
  )
(define (history-answer-v1 history)
  (append '(earlier you said that) (pick-random history))
  )
  
; 1й способ генерации ответной реплики -- замена лица в реплике пользователя и приписывание к результату нового начала
(define (qualifier-answer user-response)
        (append (pick-random '((you seem to think that)
                               (you feel that)
                               (why do you believe that)
                               (why do you say that)
                               (you think that)
                               (are you sure that)
                               (why do you think that)
                               )
                )
                (change-person-v1 user-response)
        )
 )

; случайный выбор одного из элементов непустого списка lst
(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

; Задание 1
; замена лица во фразе

(define (change-person phrase)
        (many-replace-v3
		'((am are)
        (are am)
        (i you)
        (me you)
        (mine yours)
        (my your)
        (myself yourself)
        (you i)
        (your my)
        (yours mine)
        (yourself myself)
        (we you)
        (us you)
        (our your)
        (ours yours)
        (ourselves yourselves)
        (yourselves ourselves)
        (shall will))
                      phrase)
 )
(define (form-change-lst lst)
  (define (form-sub-lst lst-1 chage-to)
    (let loop_1 ( (result '()) (llst-1 lst-1) (tail-lst-1 (cdr lst-1)) (chage-to chage-to))
      (cond
        [(eq? tail-lst-1 '()) (append (list(list (car llst-1) chage-to)) result)]
        [else
         (loop_1 (append (list(list (car llst-1) chage-to)) result) tail-lst-1 (cdr tail-lst-1) chage-to )]
    )))
  (if (= 1 (cadr(cdr lst))) (append (form-sub-lst (car lst) (car(cadr lst)))
                                    (let reverse-lst ((result '()) (llst (car lst)) (tail-lst (cdr(car lst))))
                                      (cond
                                        [(eq? tail-lst '()) (append (form-sub-lst (cadr lst) (car llst)) result)]
                                        [else
                                         (reverse-lst (append (form-sub-lst (cadr lst) (car llst)) result) tail-lst (cdr tail-lst))]
                                        )))
      (form-sub-lst (car lst) (car(cadr lst))))
  )
(define (change-person-v1 phrase)
  (many-replace-v1(append ;переделать с использованием foldr и apply
   (form-change-lst '( (are) (am) 1))
   (form-change-lst '( (i me we us) (you) 0))
   (form-change-lst '( (mine ours) (yours) 0))
   (form-change-lst '( (my our) (your) 1))
   (form-change-lst '( (yourself) (myself) 1))
   (form-change-lst '( (you) (i) 0))
   (form-change-lst '( (your) (my) 0))
   (form-change-lst '( (yours) (mine) 0))
   (form-change-lst '((ourselves) (yourselves) 1))
   (form-change-lst '((shall) (will) 0)) 
   ) phrase)
  )
(define change-words '(
                       ((are) (am) 1)
                       ((i me we us) (you) 0)
                       ((mine ours) (yours) 0)
                       ((my our) (your) 1)
                       ((yourself) (myself) 1)
                       ((you) (i) 0)
                       ((your) (my) 0)
                       ((yours) (mine) 0)
                       ((ourselves) (yourselves) 1)
                       ((shall) (will) 0)))
(define (chage-person-v2 phrase)
 (foldr (lambda (v l) (append (form-change-lst v) l)) '() change-words)
)
; осуществление всех замен в списке lst по ассоциативному списку replacement-pairs
(define (many-replace replacement-pairs lst)
        (cond ((null? lst) lst)
              (else (let ((pat-rep (assoc (car lst) replacement-pairs))) ; Доктор ищет первый элемент списка в ассоциативном списке замен
                      (cons (if pat-rep (cadr pat-rep) ; если поиск был удачен, то в начало ответа Доктор пишет замену
                                (car lst) ; иначе в начале ответа помещается начало списка без изменений
                            )
                            (many-replace replacement-pairs (cdr lst)) ; рекурсивно производятся замены в хвосте списка
                        )
                     )
               )
         )
)
(define (select-exchange? replacement-pairs lst) 
  (let ((pat-rep (assoc (car lst) replacement-pairs)))
    (if pat-rep pat-rep #f)
    )
  )
(define (many-replace-v1 replacement-pairs lst)
  (cond
    [(null? lst) lst]
    [(select-exchange? replacement-pairs lst) (cons (cadr(select-exchange? replacement-pairs lst)) (many-replace-v1 replacement-pairs (cdr lst)))]
    [else (cons (car lst) (many-replace-v1 replacement-pairs (cdr lst)))]
     
        )
  )

(define (many-replace-v2 replacement-pairs lst)
  (if (null? lst) lst
      (reverse(let sub-function ((lst lst) (pat-rep (assoc (car lst) replacement-pairs)) (result '()))
                (cond
                  [(null? lst) result]
                  [(and pat-rep (null?(cdr lst))) (cons (cadr pat-rep) result)] 
                  [pat-rep (sub-function (cdr lst) (assoc (cadr lst) replacement-pairs) (cons (cadr pat-rep) result))]
                  [(null?(cdr lst)) (cons (car lst) result )]
                  [else
                   (sub-function (cdr lst) (assoc (cadr lst) replacement-pairs) (cons (car lst) result ))]
                  )
                )
              )
      )
  )

(define (many-replace-v3 replacement-pairs lst)
  (map (lambda (element)
                 (let ((pat-rep (assoc element replacement-pairs)))
                   (if pat-rep (cadr pat-rep) element))
                 )lst)
  )
  
; 2й способ генерации ответной реплики -- случайный выбор одной из заготовленных фраз, не связанных с репликой пользователя
(define (hedge-answer)
       (pick-random '((please go on)
                       (many people have the same sorts of feelings)
                       (many of my patients have told me the same thing)
                       (please continue)
                       (please be more specific)
                       (it is interesting)
                       )
         )
)

(visit-doctor-v1 'suppertime 3)
;(change-person-v1 '(whenever i enter a room no one will look me in the eye))
;(reply-v3 reply-strategies '(i am eblan in scheme) '())
;(reply-v2 '(scheme asd father scala) '())
;(history-answer-v1 '((scheme asd father scala)))
;(pick-random '((scheme asd father scala) (asd)))
;(context-answer '(scheme asd father scala) '())
;(reply-v3 '() '() methods)
;(reply-v3 reply-strategies '(scheme asd father scala) '())