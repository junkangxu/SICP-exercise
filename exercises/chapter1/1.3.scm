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



;;;;;;;;;;
;;;Ex 1.35
;;;;;;;;;;
; x -> 1 + 1/x
; => multiply both side by x x^2 -> x + 1, which is the definition of phi
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
; result -> 1.618



;;;;;;;;;;
;;;Ex 1.36
;;;;;;;;;;
(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (display guess)
      (newline)
      (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))

(define (x-to-the-x y)
  (fixed-point (lambda (x) (/ (log y) (log x)))
    1000))



;;;;;;;;;;
;;;Ex 1.37
;;;;;;;;;;
; a)
(define (cont-frac n d k)
  (cond ((= k 0) 0)
        (else (/ (n k) (+ (d k) (cont-frac n d (- k 1)))))))

; b)
(define (cont-frac n d k)
  (define (iter a result)
    (if (= a 0)
        result
        (iter (- a 1)
              (/ (n a) (+ (d a) result)))))
  (iter k 0))
; both method need 10 terms to keep error less than 0.0001



;;;;;;;;;;
;;;Ex 1.38
;;;;;;;;;;
(define (eular k)
  (+ 2.0 (cont-frac (lambda (x) 1)
                    (lambda (x)
                      (if (= (remainder x 3) 2)
                          (/ (+ i 1) 1.5)
                          1))
                    k)))



;;;;;;;;;;
;;;Ex 1.39
;;;;;;;;;;
(define (tan-cf x k)
  (define (iter a result)
    (if (= a 0)
        result
        (iter (- a 1)
              (/ (if (= a 1) x (square x))
                 (- (- (* 2 a) 1)
                 result)))))
  (iter k 0) )



;;;;;;;;;;
;;;Ex 1.40
;;;;;;;;;;
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))



;;;;;;;;;;
;;;Ex 1.41
;;;;;;;;;;
(define (double f)
  (lambda (x) (f (f x))))



;;;;;;;;;;
;;;Ex 1.42
;;;;;;;;;;
(define (compose f g x)
  (lambda (x) (f (g x))))



;;;;;;;;;;
;;;Ex 1.43
;;;;;;;;;;
(define (repeat f n)
  (if (< n 1)
      (lambda (x) x)
      (compose f (repeat f (- n 1)))))


;;;;;;;;;;
;;;Ex 1.44
;;;;;;;;;;
(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3 )))

(define (n-fold-smooth f n) ((repeated smooth n) f))



;;;;;;;;;;
;;;Ex 1.45
;;;;;;;;;;
(define (get-max-pow n)
   (define (iter p r)
     (if (< (- n r) 0)
         (- p 1)
         (iter (+ p 1) (* r 2))))

   (iter 1 2))

 (define (pow b p)
   (define (even? x)
     (= (remainder x 2) 0))

   (define (sqr x)
     (* x x))

   (define (iter res a n)
     (if (= n 0)
         res
         (if (even? n)
             (iter res (sqr a) (/ n 2))
             (iter (* res a) a (- n 1)))))

   (iter 1 b p))

 (define (nth-root n x)
   (fixed-point ((repeated average-damp (get-max-pow n))
                 (lambda (y) (/ x (pow y (- n 1)))))
                1.0))



(define (close-enough? v1 v2)
  (define tolerance 1.e-6)
  (< (/ (abs (- v1 v2)) v2)  tolerance))

(define (iterative-improve improve close-enough?)
  (lambda (x)
    (let ((xim (improve x)))
      (if (close-enough? x xim)
          xim
          ((iterative-improve improve close-enough?) xim))
    )))
; a) rewrite sqrt using iterative-improve
(define (sqrt x)
  ((iterative-improve
    (lambda (y)
      (/ (+ (/ x y) y) 2))
    close-enough?) 1.0))
; b) rewrite fixed-point using iterative-improve
(define (fixed-point f first-guess)
  ((iterative-improve 
    f
    close-enough?) first-guess))
