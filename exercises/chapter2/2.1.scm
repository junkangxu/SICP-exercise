;;;;;;;;;
;;;Ex 2.1
;;;;;;;;;
; assume d will never be 0
(define (negative n) (* -1 n))

(define (make-rat n d)
  (cond ((= n 0)  0)
        ((and (< n 0) (> d 0)) (cons n d))
        ((and (> n 0) (< d 0)) (cons (negative n) (negative d)))
        ((and (< n 0) (< d 0)) (cons (negative n) (negative d)))
        ((and (> n 0) (> d 0)) (cons n d))))
