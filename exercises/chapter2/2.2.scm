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
