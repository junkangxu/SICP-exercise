;;;;;;;;;;
;;;Ex 2.53
;;;;;;;;;;
(list 'a 'b 'c) ; => (a b c)
(list (list 'george)) ; => ((george))
(cdr '((x1 x2) (y1 y2))) ; => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; => (y1 y2)
(pair? car '(a short list)) ; => #f
(memq 'red '((red shoes) (blue socks))) ; => #f
(memq 'red '(red shoes blue socks)) ; => (red shoes blue socks)



;;;;;;;;;;
;;;Ex 2.54
;;;;;;;;;;
(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2)) (and (equal? (car list1) (car list2))
         (equal? (cdr list1) (cdr list2))))
        (else #f)))



;;;;;;;;;;
;;;Ex 2.55
;;;;;;;;;;
(car ''abracadabra)
; ''abracadabra => a literal of 'abracadabra
; therefore first element of 'abracadabra is ', which is a quote



;;;;;;;;;;
;;;Ex 2.56
;;;;;;;;;;
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponentiation base exp)
  (cond ((=number? base 1) 1)
        ((=number? exp 1) base)
        ((=number? exp 0) 1)
        (else (list '** base exp))))



;;;;;;;;;;
;;;Ex 2.57
;;;;;;;;;;
(define (make-sum-list l) 
  (if (= (length l) 2) 
      (list '+ (car l) (cadr l)) 
      (make-sum (car l) (make-sum-list (cdr l)))))

(define (make-product-list l) 
  (if (= (length l) 2) 
      (list '* (car l) (cadr l)) 
      (make-product (car l) (make-product-list (cdr l))))) 
 
(define (augend s) 
  (let ((a (cddr s))) 
    (if (= (length a) 1) 
        (car a) 
        (make-sum-list a)))) 

(define (multiplicand p) 
  (let ((m (cddr p))) 
    (if (= (length m) 1) 
        (car m) 
        (make-product-list m)))) 



;;;;;;;;;;
;;;Ex 2.58
;;;;;;;;;;
(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier x) (car x))
(define (multiplicand x) (caddr x))

(define (make-product m1 m2)
  (cond ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((or (=number? m1 0) (=number? m2 0)) 0)
        (else (list m1 '* m2))))



;;;;;;;;;;
;;;Ex 2.59
;;;;;;;;;;
(define (union-set s1 s2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))



;;;;;;;;;;
;;;Ex 2.60
;;;;;;;;;;
; element-of-set? is the same
; intersection-set is the same
(define (adjoin-set x set) (cons x set))
(define (union-set set1 set2) (append set1 set2))



;;;;;;;;;;
;;;Ex 2.61
;;;;;;;;;;
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))



;;;;;;;;;;
;;;Ex 2.62
;;;;;;;;;;
(define (union-set set1 set2)
  (if (and (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
              ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
              ((> x1 x2) (cons x2 (union-set set1 (cdr set2))))))))



;;;;;;;;;;
;;;Ex 2.63
;;;;;;;;;;
; a) both function will produce the same result
; b) first method: 2 * T(n/2) + O(n/2) => T(n) = O(nlogn)
;    second method: 2 * T(n/2) + O(1) => T(n) = O(n)



;;;;;;;;;;
;;;Ex 2.64
;;;;;;;;;;
; a) find the median element as the root, and built partial tree based on
;    left subtree and right subtree
;          5
;        /   \
;       1    9
;       \   / \
;       3  7  11
;
; b) split the original list and combine the new result both take constant time
;    and we need to traverse all nodes. Therefore, O(n)



;;;;;;;;;;
;;;Ex 2.65
;;;;;;;;;;
(define (union-set tree1 tree2) 
  (list->tree (union-set-list (tree->list tree1) (tree->list tree2))))

(define (intersection-set tree1 tree2)
  (list->tree (intersection-set-list (tree->list tree1) (tree->list tree2))))



;;;;;;;;;;
;;;Ex 2.66
;;;;;;;;;;
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))



;;;;;;;;;;
;;;Ex 2.67
;;;;;;;;;;
; (A | 4) (B | 2) (D | 1) (C | 1)
; => (A \ 4) (B | 2) (DC | 2)
; => (A | 4) (BDC | 4)
; => (ABDC | 8)
;
;       {ABDC} 8
;        /   \
;    {A} 4 {BDC} 4
;           /  \
;       {B} 2  {DC} 2
;              /  \
;          {D} 1 {C} 1
;
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
; => A (1 1 0 0 1 0 1 0 1 1 1 0)
; => AD (0 1 0 1 0 1 1 1 0)
; => ADA (1 0 1 0 1 1 1 0)
; => ADAB (1 0 1 1 1 0)
; => ADABB (1 1 1 0)
; => ADABBC (0)
; => ADABBCA



;;;;;;;;;;
;;;Ex 2.68
;;;;;;;;;;
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
  ((member symbol (symbols tree))
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (if (member symbol (symbols left))
          (cons 0 (encode-symbol symbol left))
          (cons 1 (encode-symbol symbol right)))))
  (else (error "bad symbol -- ENCODE-SYMBOL" symbol))))



;;;;;;;;;;
;;;Ex 2.69
;;;;;;;;;;
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                        (cddr leaves)))))



;;;;;;;;;;
;;;Ex 2.70
;;;;;;;;;;
; (NA | 16) (YIP | 9) (SHA | 3) (A | 2) (GET | 2) (JOB | 2) (BOOM | 1) (WAH | 1)
; => (NA | 16) (YIP | 9) (SHA | 3) (A | 2) (GET | 2) (JOB | 2) (BOOM WAH | 2)
; => (NA | 16) (YIP | 9) (SHA | 3) (A GET | 4) (JOB BOOM WAH | 4)
; => (NA | 16) (YIP | 9) (SHA A GET | 7) (JOB BOOM WAH | 4)
; => (NA | 16) (SHA A GET JOB BOOM WAH | 11) (YIP | 9)
; => (NA | 16) (SHA A GET JOB BOOM WAH YIP | 20)
; => (NA SHA A GET JOB BOOM WAH YIP | 36)
;
;               {NA SHA A GET A JOB BOOM WAH TIP} 36
;                  /                        \
;             {NA} 16         {SHA A GET JOB BOOM WAH YIP} 20
;                                 /                   \
;                    {SHA A GET JOB BOOM WAH} 11     {YIP} 9
;                        /              \
;             {SHA A GET} 7        {JOB BOOM WAH} 4
;                /      \             /        \
;           {SHA} 3   {A GET} 4   {JOB} 2   {BOOM WAH} 2
;                      /     \               /      \
;                  {A} 2   {GET} 2       {BOOM} 1   {WAH} 1
;
; A: 10010    NA: 0       BOOM: 10110    SHA: 1000    GET: 10011
; YIP: 11     JOB: 1010   WAH: 10111
;
; Get a job => 10011 10010 1010    (14)
; Sha na na na na na na na na => 1000 0 0 0 0 0 0 0 0    (12)
; Wah yip yip yip yip yip yip yip yip yip => 10111 11 11 11 11 11 11 11 11 11    (23)
; Sha boom => 1000 10110    (9)
;
; 14 * 2 + 12 * 2 + 23 + 9 = 84
;
; For fixed-length encoding
; 8 distinct words => 3 bits for every word
; 3 * # of words in the song = 3 * 36 = 108



;;;;;;;;;;
;;;Ex 2.71
;;;;;;;;;;
; suppose n = 5
; we say we have symbols (A | 16) (B | 8) (C | 4) (D | 2) (E | 1)
; => (A | 16) (B | 8) (C | 4) (DE | 3)
; => (A | 16) (B | 8) (CDE | 7)
; => (A | 16) (BCDE | 15)
; => (ABCDE | 31)
;
;           {ABCDE} 31
;            /      \
;        {A} 16   {BCDE} 15
;                  /     \
;              {B} 8   {CDE} 7
;                       /   \
;                   {C} 4   {DE} 3
;                            /   \
;                         {D} 2  {E} 1
;
; suppose n = 10
; we say we have symbols
; (A | 512) (B | 256) (C | 128) (D | 64) (E | 32) (F | 16) (G | 8) (H | 4) (I | 2) (J | 1)
; => (A | 512) (B | 256) (C | 128) (D | 64) (E | 32) (F | 16) (G | 8) (H | 4) (IJ | 3)
; => (A | 512) (B | 256) (C | 128) (D | 64) (E | 32) (F | 16) (G | 8) (HIJ | 7)
; => (A | 512) (B | 256) (C | 128) (D | 64) (E | 32) (F | 16) (GHIJ | 15)
; => (A | 512) (B | 256) (C | 128) (D | 64) (E | 32) (FGHIJ | 31)
; => (A | 512) (B | 256) (C | 128) (D | 64) (EFGHIJ | 63)
; => (A | 512) (B | 256) (C | 128) (DEFGHIJ | 127)
; => (A | 512) (B | 256) (CDEFGHIJ | 255)
; => (A | 512) (BCDEFGHIJ | 511)
; => (ABCDEFGHIJ | 1023)
;
;            {ABCDEFGHIJ} 1023
;               /           \
;          {A} 512    {BCDEFGHIJ} 511
;                        /        \
;                   {B} 256  {CDEFGHIJ} 255
;                               /        \
;                           {C} 128   {DEFGHIJ} 127
;                                       /       \
;                                   {D} 64   {EFGHIJ} 63
;                                              /      \
;                                          {E} 32   {FGHIJ} 31
;                                                     /    \
;                                                {F} 16  {GHIJ} 15
;                                                         /      \
;                                                      {G} 8   {HIJ} 7
;                                                               /    \
;                                                            {H} 4  {IJ} 3
;                                                                    /  \
;                                                                {I} 2  {J} 1
;
; minimum number of bits to contruct a symbol is 1 for symbol of weight 2^n - 1
; maximum number of bits to contruct a symbol is n-1 for symbol of weight 1



;;;;;;;;;;
;;;Ex 2.72
;;;;;;;;;;
; most frequent symbol: O(2^n)
; least frequent symbol: O(1)