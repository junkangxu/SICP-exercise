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

(test 0 (p))
; applicative-order: infinite loop
; normal-order: 0 == 0 => true => return 0



;;;;;;;;;
;;;Ex 1.6
;;;;;;;;;
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
; The new-if have to evaluate the else-clause which is in the iteration
; therefore, it will keep evaluate permanently



;;;;;;;;;
;;;Ex 1.7
;;;;;;;;;
(define (good-enough? guess x)
  (< (abs(- (improve guess x) guess))
     (* 0.001 guess)))



;;;;;;;;;
;;;Ex 1.8
;;;;;;;;;
; helper functions
(define (average-of-3 a b c) (/ (a + b + c) 3))
(define (improve guess x) (average-of-3 (/ x (* guess guess)) guess guess ))
(define (good-enough? guess prev-guess)
  (< (abs (- guess prev-guess)) (abs (* guess 0.001))))
; main function
(define (cubic-root-iter guess prev-guess x)
  (if (good-enough? guess prev-guess)
      guess
      (cubic-root-iter (improve guess x) guess x)))
; caller function
(define (cubic-root x)
  ((if (< x 0) - +) (cubic-root-iter (improve 1.0 (abs x)) 1 (abs x))))
; examples
(cubic-root 1)
(cubic-root -8)
(cubic-root 27)
