;;;;;;;;;;
;;;Ex 2.17
;;;;;;;;;;
(define (last-pair items)
  (let ((rest (cdr items)))
  (if (null? rest)
      items
      (last-pair rest))))



;;;;;;;;;;
;;;Ex 2.18
;;;;;;;;;;
(define (reverse items)
  (if (null? (cdr items))
      items
      (append (reverse(cdr items))
              (car items))))



;;;;;;;;;;
;;;Ex 2.19
;;;;;;;;;;
(define (first-denomination denominations) (car denominations))
(define (except-first0denomination denominations) (cdr denominations))
(define (no-more? denominations) (null? denominations))
; the order does not affect the result



;;;;;;;;;;
;;;Ex 2.20
;;;;;;;;;;
(define (same-parity first . rest)
  (let ((first-parity even? first)))
  (define (iter x res)
    (if (null? cdr(x))
        (if (= first-parity (even? (car x)))
            (append res x)
            (res))
        (if (= first-parity (even? (car x)))
            (iter (cdr x) (append res x))
            (iter (cdr x) res)) ))
  (iter rest nil))



;;;;;;;;;;
;;;Ex 2.21
;;;;;;;;;;
; method 1
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))
; method 2
(define (square-list items) (map square items))



;;;;;;;;;;
;;;Ex 2.22
;;;;;;;;;;
; the first add first item to the end everytime
; the second iterative method returns list everytime, so we have list of list of list ...



;;;;;;;;;;
;;;Ex 2.23
;;;;;;;;;;
(define (for-each proc items)
  (proc (car items))
  (if (null? (cdr items))
      (true)
      (for-each proc cdr(items))))



;;;;;;;;;;
;;;Ex 2.24
;;;;;;;;;;
; graph at ../../img/Ex 2.24.jpeg



;;;;;;;;;;
;;;Ex 2.25
;;;;;;;;;;
; (1)
(car (cdr (car (cdr (cdr items)))))
; (2)
(car (car items))
; (3)
(car (cdr (cdr (cdr (cdr (cdr (cdr items)))))))



;;;;;;;;;;
;;;Ex 2.26
;;;;;;;;;;
(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y)
; => (1 2 3 4 5 6)
(cons x y)
; => ((1 2 3) 4 5 6)
(list x y)
; => ((1 2 3) (4 5 6))


;;;;;;;;;;
;;;Ex 2.27
;;;;;;;;;;
(define (deep-reverse x)
  (if (pair? x)
      (append (deep-reverse (cdr x))
              (list (deep-reverse (car x))))
      x))



;;;;;;;;;;
;;;Ex 2.28
;;;;;;;;;;
(define (fringe x)
  (cond ((null? x) x)
        ((number? x) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))



;;;;;;;;;;
;;;Ex 2.29
;;;;;;;;;;
; (a)
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cdr branch))
; (b)
(define (total-weight mobile)
  (if (pair? (branch-structure mobile)))
      (+ (total-weight (left-branch mobile))
         (total-weight (right-branch mobile)))
      (branch-structure mobile))
; (c)
(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

(define (balanced? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (branch-torque left)
            (branch-torque right))
         (branch-balanced? left)
         (branch-balanced? right))))
; (d)
; Do not have to change



;;;;;;;;;;
;;;Ex 2.30
;;;;;;;;;;
; direct method
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; map method
(define (square-tree tree)
  (map (lambda (sub-tree)
        (if (pair? sub-tree)
            (square-tree sub-tree)
            (square sub-tree)))
        tree))



;;;;;;;;;;
;;;Ex 2.31
;;;;;;;;;;
(define (tree-map proc tree)
  (map (lambda (sub-tree)
          (cond ((null? sub-tree) nil)
                ((not (pair? sub-tree)) (proc tree))
                (else (tree-map proc sub-tree))))
        tree))



;;;;;;;;;;
;;;Ex 2.32
;;;;;;;;;;
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))



;;;;;;;;;;
;;;Ex 2.33
;;;;;;;;;;
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))



;;;;;;;;;;
;;;Ex 2.34
;;;;;;;;;;
(define (horner-eval x coefficient-sequence)
   (accumulate (lambda (this-coeff higher-terms)
                 (+ (* higher-terms x) this-coeff))
               0
               coefficient-sequence))



;;;;;;;;;;
;;;Ex 2.35
;;;;;;;;;;
(define (count-leaves-recursive t)
  (accumulate + 0
    (map
      (lambda (t)
        (cond ((null? t) 0)
              ((pair? t) (count-leaves-recursive t))
              (else 1)))
      t)))



;;;;;;;;;;
;;;Ex 2.36
;;;;;;;;;;
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))



;;;;;;;;;;
;;;Ex 2.37
;;;;;;;;;;
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
            (map (lambda (col)
                    (dot-product row col))
                 n-cols))
         m)))



;;;;;;;;;;
;;;Ex 2.38
;;;;;;;;;;
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(fold-right / 1 (list 1 2 3))
; (/ 1 (/ 2 (/ 3 1)))
; => (/ 1 (/ 2 3))
; => (/ 1 2/3)
; => 3/2
(fold-left / 1 (list 1 2 3))
; (/ (/ (/ 1 1) 2) 3)
; => (/ (/ 1 2) 3)
; => (/ 1/2 3)
; => 1/6
(fold-right list nil (list 1 2 3))
; (list 1 (list 2 list (3 nil)))
; => (list 1 (list 2 (list 3 nil)))
; => (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))
; (list (list (list nil 1) 2) 3)
; => (((()) 1) 2) 3)



;;;;;;;;;;
;;;Ex 2.39
;;;;;;;;;;
; using fold-right
(define (reverse sequence)
  (fold-right (lambda (x y)
                (append (list x) y))
              nil
              sequence))
; using fold-left
(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x))
             nil
             sequence))



;;;;;;;;;;
;;;Ex 2.40
;;;;;;;;;;
(define (unique-pairs n)
  (flatmap (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))



;;;;;;;;;;
;;;Ex 2.41
;;;;;;;;;;
(define (ordered-triples-sum n s)
  (filter (lambda (list) (= (accumulate + 0 list) s))
         (flatmap
          (lambda (i)
            (flatmap (lambda (j)
                 (map (lambda (k) (list i j k))
                      (enumerate-interval 1 (- j 1))))
                 (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n))))



;;;;;;;;;;
;;;Ex 2.42
;;;;;;;;;;
