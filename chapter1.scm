;;;;;;;;;
;;;Ex 1.1
;;;;;;;;;
10
; 10
(+ 5 3 4)
; 5 + 3 + 4 =12
(- 9 1)
; 9 - 1 = 8
(/ 6 2)
; 6 / 2 = 3
(+ (* 2 4) (- 4 6))
; (2 * 4) + (4 - 6) = 8 - 2 = 6
(define a 3)
; a = 3
(define b (+ a 1))
; b = a + 1 = 3 + 1 = 4
(+ a b (* a b))
; a + b + (a * b) = 3 + 4 + (3 * 4) = 19
(= a b)
; a == b => 3 == 4 => false =？ #f
(if (and (> b a) (< b (* a b)))
  b
  a)
; b > a => 4 > 3 => True
; b < (a * b) => 4 < 12 => True
; => b = 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
; a == 4 => 3 == 4 => false
; b == 4 => 4 == 4 => true
; 6 + 7 + a = 6 + 7 + 3 = 16
(+ 2 (if (> b a) b a))
; b > a => 4 > 3 => true
; 2 + b = 2 + 4 = 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; a > b => 3 > 4 => false
; a < b => 3 < 4 => true
; b * (a + 1) = 4 * (3 + 1) = 16



;;;;;;;;;
;;;Ex 1.2
;;;;;;;;;
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;;;;;;;;
;;;Ex 1.3
;;;;;;;;;
(define (<= x y) (not (< x y)))

(define (sum-bigger-two a b c)
  (cond ((and (<= a b) (<= a c)) (+ b c))
        ((and (<= b a) (<= b c)) (+ a c))
        ((and (<= c a) (<= c b)) (+ a b))
  )
)



;;;;;;;;;
;;;Ex 1.4
;;;;;;;;;
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; if b > 0  => a + b
; if b <= 0 => a - b
; return a + |b|



;;;;;;;;;
;;;Ex 1.5
;;;;;;;;;
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
