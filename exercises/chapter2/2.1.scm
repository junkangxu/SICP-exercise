;;;;;;;;;
;;;Ex 2.1
;;;;;;;;;
; assume d will never be 0
(define (negative n) (* -1 n))

(define (standard-rat n d)
  (cond ((= n 0)  0)
        ((and (< n 0) (> d 0)) (cons n d))
        ((and (> n 0) (< d 0)) (cons (negative n) (negative d)))
        ((and (< n 0) (< d 0)) (cons (negative n) (negative d)))
        ((and (> n 0) (> d 0)) (cons n d))))

(define (make-rat n d)
  (let g (abs (gcd n d)))
  (let s-rat (standard-rat))
  (cons (/ (numer s-rat) g) (/ (denom s-rat) g)))



;;;;;;;;;
;;;Ex 2.2
;;;;;;;;;
(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (make-segment start-point end-point) (cons start-point end-point))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (midpoint-segment segment)
  (define (average a b) (/ (- a b) 2.0))
  (let start-x (x-point (start-point segment)))
  (let start-y (y-point (start-point segment)))
  (let end-x (x-point (end-point segment)))
  (let end-y (y-point (end-point segment)))
  (make-point (average end-x start-x) (average end-y start-y)))

(define (print-point p)
  (newline)
  (display "()")
  (display (x-point p))
  (display ","
  (display (y-point p))
  (display ")"))



;;;;;;;;;
;;;Ex 2.3
;;;;;;;;;
;;; Method 1
; constructor
(define (make-rect segment-x segment-y)
  (cons segment-x segment-y))
; selector
(define (segment-x rect) (car rect))
(define (segment-y rect) (cdr rect))
; public method
(define (area rect) (* (segment-x rect) (segment-y rect)))
(define (perimeter rect) (* 2 (+ (segment-x rect) (segment-y rect))))
;;; Method 2
; constructor
(define (make-rect bottom-left length height)
  (cons bottom-left (cons length height)))
; selector
(define (bottom-left rect) (car rect))
(define (length rect) (car (cdr rect)))
(define (height rect) (cdr (cdr rect)))
; public method
(define (area rect) (* (length rect) (height rect)))
(define (perimeter rect) (* 2 (+ (length rect) (height rect))))



;;;;;;;;;
;;;Ex 2.4
;;;;;;;;;
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))



;;;;;;;;;
;;;Ex 2.5
;;;;;;;;;
; helper functions
(define (divides? a b)
   (= 0 (remainder b a)))

(define (count-0-remainder-divisions n divisor)
  (define (iter x divisions)
    (if (divides? divisor x)
        (iter (/ x divisor) (+ divisions 1))
        divisions))
  (iter n 0))
; constructor
(define (my-cons a b) (* (exp 2 a) (exp 3 b)))
; selector
(define (my-car z) (count-0-remainder-divisions z 2))
(define (my-cdr z) (count-0-remainder-divisions z 3))



;;;;;;;;;
;;;Ex 2.6
;;;;;;;;;
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
; more explanation on
; http://community.schemewiki.org/?sicp-ex-2.6



;;;;;;;;;
;;;Ex 2.7
;;;;;;;;;
(define (lower-bound interval) (min (car interval) (cdr interval)))
(define (upper-bound interval) (max (car interval) (cdr interval)))



;;;;;;;;;
;;;Ex 2.8
;;;;;;;;;
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))



;;;;;;;;;
;;;Ex 2.9
;;;;;;;;;
; complete proof in
; http://community.schemewiki.org/?sicp-ex-2.9



;;;;;;;;;;
;;;Ex 2.10
;;;;;;;;;;
(define (div-interval x y)
  (if (<= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y)
      (mul-interval x
                    (make-interval (/ 1. (upper-bound y))
                                   (/ 1. (lower-bound y))))))



;;;;;;;;;;
;;;Ex 2.11
;;;;;;;;;;
(define (mul-interval x y)
  (define (positive? x) (>= x 0))
  (define (negative? x) (< x 0))
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
  (cond ((and (positive? xl) (positive? yl)) (make-interval (* xl yl) (* xu yu)))
        ((and (positive? xl) (negative? yl)) (make-interval (* xu yl) (* (if (negative? yu) xl xu) yu)))
        ((and (negative? xl) (positive? yl)) (make-interval (* xl yu) (* xu (if (negative? xu) yl yu))))
        ((and (positive? xu) (positive? yu))
          (let ((l (min (* xl yu) (* xu yl)))
                (u (max (* xl yl) (* xu yu))))
          (make-interval l u)))
        ((and (positive? xu) (negative? yu)) (make-interval (* xu yl) (* xl yl)))
        ((and (negative? xu) (positive? yu)) (make-interval (* xl yu) (* xl yl)))
        (else (make-interval (* xu yu) (* xl yl))))))



;;;;;;;;;;
;;;Ex 2.12
;;;;;;;;;;
(define (make-center-percent center percent)
  (let ((width (* c (/ percent 100.0)))))
  (make-interval (- c width) (+ c width)))

(define (percent interval)
  (let ((center (/ (+ (upper-bound interval) (lower-bound interval)) 2))))
  (/ (- (upper-bound interval) center) center))



;;;;;;;;;;
;;;Ex 2.13
;;;;;;;;;;
; complete proof at ../../img/Ex 2.13.jpeg



;;;;;;;;;;
;;;Ex 2.14
;;;;;;;;;;
; A/A shoud be 1.0
; suppose A = [start, end] and B = [start, end]
; even if A and B are in the same interval, A/B cannot be 1.0 easily
; therefore A/A may not be 1.0 as we expect



;;;;;;;;;;
;;;Ex 2.15
;;;;;;;;;;
; Eva is correct, since we do not introduce same interval multiple times



;;;;;;;;;;
;;;Ex 2.16
;;;;;;;;;;
; Traditional arithmetic operations will not work in interval calculations
; it is really hard to devise an interval calculation package
; maybe not impossible, but really hard
