;;;;;;;;;;
;;;Ex 2.73
;;;;;;;;;;
; (a)
; number? and same-variables? are predicates, which mean they have nothing to dispatch
; (b)
(define (install-sum-package)
  (define (sum-deriv expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))
  (define (addend expr) (car expr))
  (define (augend expr) (cadr expr))
  (define (make-sum x1 x2)
    (cond ((and (number? x1) (number? x2)) (+ x1 x2))
          ((=number? x1 0) x2)
          ((=number? x2 0) x1)
          (else (list '+ x1 x2))))
  (define (mul-deriv expr var)
    (make-sum (make-product (multiplier expr)
                            (deriv (multiplicand expr) var))
              (make-product (multiplicand expr)
                            (deriv (multiplier expr) var))))
  (define (multiplier expr) (car expr))
  (define (multiplicand expr) (cadr expr))
  (define (make-product x1 x2)
    (cond ((and (number? x1) (number? x2)) (* x1 x2))
          ((=number? x1 1) x2)
          ((=number? x2 1) x1)
          ((or (=number? x1 0) (=number? x2 0)) 0)
          (else (list '* x1 x2)))))

(put 'deriv '+ sum-deriv)
(put 'deriv '* mul-deriv)
; (c)
(define (exponentiation-deriv expr var)
  (make-product (exponent expr)
                (make-product (make-exponentiation (base exprt)
                                                   (make-sum (exponent epxr) -1))
                              (deriv (base expr) var))))

(define (exponent expr) (cadr expr))

(define (base expr) (car expr))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        (else (list '** base exponent))))

(put 'deriv '** exponentiation-deriv)
; (d)
; change the order of arguments in function "put"



;;;;;;;;;;
;;;Ex 2.74
;;;;;;;;;;
; (a)
(define (get-record division employee-name)
  ((get division 'record) employee-name))
; (b)
(define (get-salary division record)
  ((get division 'salary) record))
; (c)
(define (find-employee-record employee-name division-list)
  (if (null? division-list)
      #f
      (or (get-record (car division-list) employee-name)
          (find-employee-record employee-name (cdr division-list)))))
; (d)
; must install get-record and get-salary functions in the new installation



;;;;;;;;;;
;;;Ex 2.75
;;;;;;;;;;
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op --- MAKE-FROM-MAG-ANG" op))))
  dispatch)



;;;;;;;;;;
;;;Ex 2.76
;;;;;;;;;;
; Message-passing style is the most appropriate when adding new types
; Data-directed style is the most appropriate when adding operations
