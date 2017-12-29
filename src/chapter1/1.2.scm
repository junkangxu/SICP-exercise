;;;;;;;;;
;;;Ex 1.9
;;;;;;;;;
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b)))
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8);
; 9
; => recursive
(define (+ a b)
  (if (= a 0)
      0
      (+ (dec a) (inc b))))
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; => iterative


;;;;;;;;;;
;;;Ex 1.10
;;;;;;;;;;
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10) ; 1024
(A 2 4)  ; 65536
(A 3 3)  ; 65536

(define (f n) (A 0 n))   ; 2n
(define (g n) (A 1 n))   ; 2^n
(define (k n) (A 2 n))   ; 2^2^n
(define (k n) (* 5 n n)) ; 5n^2


;;;;;;;;;;
;;;Ex 1.11
;;;;;;;;;;
; recursive
(define (question11-recursive n)
  (if (< n 3)
      n
      (+ (question11-recursive n-1)
         (* 2 (question11-recursive n-2))
         (* 3 (question11-recursive n-3)))))
; iterative
(define (question-iterative a b c counter)
  (if ((< counter 1) counter)
      ((= counter 0) a)
      (else (question11-iterative b c (+ a (* 2 b) (* 3 c) (- counter 1))))))



;;;;;;;;;;
;;;Ex 1.12
;;;;;;;;;;
(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))



;;;;;;;;;;
;;;Ex 1.13
;;;;;;;;;;
; statement that $Fib(n) = (a^n - b^n)/sqrt(5)$ can be proved using induction
; full proof can be found on
; https://www.evernote.com/shard/s100/sh/6a4b59d5-e99f-417c-9ef3-bcf03a4efecd/7e030d4602a0bef5df0d6dd4c2ad47bf



;;;;;;;;;;
;;;Ex 1.14
;;;;;;;;;;
; picture in ./img/Ex 1.14.jpeg
; space order of growth: O(n)
; time order of growth: O(n^2)


;;;;;;;;;;
;;;Ex 1.15
;;;;;;;;;;
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))
; a)
; (sine 12.15)
; (p (sine 4.05))
; (p (p (sine 1.35)))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine 0.05))))))
; (p (p (p (p (p 0.05)))))
; => 5 times
; b)
; every step the input is divided by log of base 3
; => order of growth = O(log(a))



;;;;;;;;;;
;;;Ex 1.16
;;;;;;;;;;
(define (square x) (* x x))
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (iter a (square b) (/ n 2)))
        (else (iter (* a b) b (- n 1)))))
(define (fast-expt b n) (fast-expt-iter 1 b n))



;;;;;;;;;;
;;;Ex 1.17
;;;;;;;;;;
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((= a 1) b)
        ((even? b) (double (* a (halve b))))
        (else (+ a (* a (- b 1))))))



;;;;;;;;;;
;;;Ex 1.18
;;;;;;;;;;
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (iter n a b)
 (cond ((= b 0) n)
       ((even? b) (iter n (double a) (halve b)))
       (else (iter (+ n a) a (- b 1)))))
(define (* a b) (iter 0 a b))



;;;;;;;;;;
;;;Ex 1.19
;;;;;;;;;;
(define (square x) (* x x))
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))



;;;;;;;;;;
;;;Ex 1.20
;;;;;;;;;;
; Normal-order :
; (gcd 206 40) => (= 40 0) = false
; (gcd 40 (reminder 206 40)) => reminder(206 40) = 6 => (= 6 0) = false
; => {1}
; (gcd (reminder 206 40) (reminder 40 (reminder 206 40)))
; => (reminder 40 (reminder 206 40)) = (reminder 40 6) = 4 => (= 4 0) = false
; => {2}
; (gcd (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40))))
; => (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))
; => (reminder 6 (reminder 40 6)) => (reminder 6 4) = 2
; => (= 2 0) = false
; => {4}
; (gcd (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))
;      (reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))
; => (reminder (reminder 40 (reminder 206 40)) (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))))
; => (reminder (reminder 40 6) (reminder 6 (reminder 40 6))
; => (reminder 4 (reminder 6 4)) => (reminder 4 2) = 0 => (= 0 0) = true
; => {7}
; (reminder (reminder 206 40) (reminder 40 (reminder 206 40)))
; => (reminder 6 (reminder 40 6)) => (reminder 6 4) = 2
; => {4}
; => {1 + 2 + 4 + 7 + 4} = {18}
; Applicative-order:
; (gcd 206 40)
; (gcd 40 (reminder 206 40)) => (gcd 40 6)
; => {1}
; (gcd 6 (reminder 40 6)) => (gcd 6 4)
; => {1}
; (gcd 4 (reminder 6 4)) => (gcd 4 2)
; => {1}
; (gcd 2 (reminder 4 2)) => (gcd 2 0) => 2
; => {1}
; => {1 + 1 + 1 + 1} = {4}



;;;;;;;;;;
;;;Ex 1.21
;;;;;;;;;;
(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7



;;;;;;;;;;
;;;Ex 1.22
;;;;;;;;;;
(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
   (cond ((> (square test-divisor) n) n)
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor n (+ test-divisor 1)))))

(define (diivides? a b) (= (reminder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
       (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes first last)
  (define (search-iter cur last)
    ((if (<= cur last) (timed-prime-test cur)) (if (<= cur last) (search-iter (+ cur 2) last)))
  (search-iter (if (even? first) (+ first 1) first)
               (if (even? last) (- last 1) last))))



;;;;;;;;;;
;;;Ex 1.23
;;;;;;;;;;
(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
; the ratio of speed is not exacly 2
; the reason could be we need to evaluate IF statement, which takes some time



;;;;;;;;;;
;;;Ex 1.24
;;;;;;;;;;
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  ; Perform the test how many times?
  ; Use 100 as an arbitrary value.
  (fast-prime? n 100))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))



;;;;;;;;;;
;;;Ex 1.25
;;;;;;;;;;
; the reminder method makes number short
; even though scheme handle aribitrary-precision arithmetic
; but long number takes longer time



;;;;;;;;;;
;;;Ex 1.26
;;;;;;;;;;
; without square we need to call expmod twice, which generate a tree recursion
; while using square we need only to call expmod once
; using expmod: O(log(n))
; not using expmod: O(n)



;;;;;;;;;;
;;;Ex 1.27
;;;;;;;;;;
(define (iter a n)
  (if (= a n)
      true
      (if (= (expmod a n n) a) (iter (+ a 1) n) false)))

(define (complete-fermat-test n) (iter 1 n))



;;;;;;;;;;
;;;Ex 1.28
;;;;;;;;;;
(define (miller-rabin n)
   (miller-rabin-test (- n 1) n))

 (define (miller-rabin-test a n)
   (cond ((= a 0) true)
         ; expmod is congruent to 1 modulo n
         ((= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n))
         (else false)))

 (define (expmod base exp m)
   (cond ((= exp 0) 1)
         ((even? exp)
          (let ((x (expmod base (/ exp 2) m)))
            (if (non-trivial-sqrt? x m) 0 (remainder (square x) m))))
         (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

 (define (non-trivial-sqrt? n m)
   (cond ((= n 1) false)
         ((= n (- m 1)) false)
         ; book reads: whose square is equal to 1 modulo n
         ; however, what was meant is square is congruent 1 modulo n
         (else (= (remainder (square n) m) 1))))
