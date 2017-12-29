;;;;;;;;;;
;;;Ex 1.29
;;;;;;;;;;
(define (cube x) (* x x x))

(define (inc n) (+ n 1))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* h k))))
  (define (simpson-term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3) (sum simpson-term 0 inc n)))



;;;;;;;;;;
;;;Ex 1.30
;;;;;;;;;;
(define (itersum term a next b)
  (define (iter a result)
          (if (> a b)
              result
              (iter (next a) (+ result (term a)))))
  (iter a 0))


;;;;;;;;;;
;;;Ex 1.31
;;;;;;;;;;
; a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)
(define (next x) (+ x 1))
(define (factorial n) (product identity 1 next n))

(define (approx-pi n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))
; b)
(define (iterprod term a next b)
  (define (iter a result)
          (if (> a b)
              result
              (iter (next a) (* result (term a)))))
  (iter a 1))



;;;;;;;;;;
;;;Ex 1.32
;;;;;;;;;;
; a)
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b) (accumulate + 0 term a next b))

(define (product term a next b) (accumulate * 1 term a next b))
; b)
(define (iter-accumulate combiner null-value term a next b)
  (define (iter a result)
          (if (> a b)
              result
              (iter (next a) (combiner result (term a)))))
  (iter a null-value))



;;;;;;;;;;
;;;Ex 1.33
;;;;;;;;;;
(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b) null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null value (filtered-accumulate combiner null-value term (next a) next b filter)))))

; a)
(define (sum-of-prime a b) (filtered-accumulate + 0 identity a inc b prime?))
; b)
(define (gcd m n)
  (cond ((< m n) (gcd n m))
        ((= n 0) m)
        (else (gcd n (reminder m n)))))

(define (relative-prime? m n) (= (gcd m n) 1))

(define (product-of-relative-prime n)
  (define (filter x) (relative-prime? x n))
  (filtered-accumulate * 1 identity 1 inc n filter))



;;;;;;;;;;
;;;Ex 1.34
;;;;;;;;;;
(define (f g) (g 2))
; (f f) => (f 2) => (2 2) => Error, 2 is not a function
