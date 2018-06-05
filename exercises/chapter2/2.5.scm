;;;;;;;;;;
;;;Ex 2.77
;;;;;;;;;;
; http://community.schemewiki.org/?sicp-ex-2.77



;;;;;;;;;;
;;;Ex 2.78
;;;;;;;;;;
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Wrong datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Wrong datum -- CONTENTS" datum))))



;;;;;;;;;;
;;;Ex 2.79
;;;;;;;;;;
(define (install-scheme-number-package)
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  (put 'equ? '(rational rational) equ?)
  'done)

(define (install-complex-package)
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
  (put 'equ? '(complex complex) equ?)
  'done)

(define (equ? x y) (apply-generic 'equ? x y))