;;;;;;;;;;
;;;Ex 2.53
;;;;;;;;;;
(list 'a 'b 'c) ; => (a b c)
(list (list 'george)) ; => ((george))
(cdr '((x1 x2) (y1 y2))) ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; => (y1 y2)
(pair? car '(a short list)) ; => #f
(memq 'red '((red shoes) (blue socks))) ; => #f
(memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)



;;;;;;;;;;
;;;Ex 2.54
;;;;;;;;;;
(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2)) (and (equal? (car list1) (car list2))
         (equal? (cdr list1) (cdr list2))))
        (else #f)))



;;;;;;;;;;
;;;Ex 2.55
;;;;;;;;;;
(car ''abracadabra)
; ''abracadabra => a literal of 'abracadabra
; therefore first element of 'abracadabra is ', which is a quote



;;;;;;;;;;
;;;Ex 2.56
;;;;;;;;;;
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 1) base)
        ((=number? exp 0) 1)
        (else (list '** base exp))))



;;;;;;;;;;
;;;Ex 2.57
;;;;;;;;;;
