(define (cddr s)
  (cdr (cdr s)))

(define (cadr s)
  (car (cdr s))
)

(define (caddr s)
  (car (cddr s))
)


(define (sign num)
  (cond ((< num 0) -1)
        ((= num 0) 0)
        (else 1)) 
)


(define (square x) (* x x))

(define (pow x y)
  (cond ((= y 0) 1)
        ((= y 1) x)
        ((even? y)
            (square (pow x (/ y 2))))
        (else
            (* x 
               (square (pow x (/ (- y 1) 2))))))
)


(define (unique s)
  (cond ((null? s) nil)
        ((null? (cdr s)) s)
        (else (cons (car s) (unique (filter (lambda (x) (not (equal? (car s) x))) (cdr s)))))
  )
)


(define (replicate x n)
    
    (define (helper x n curr_list)
        (if (= n 0)
            curr_list
            (helper x (- n 1) (cons x curr_list))
        )
    )

    (helper x n nil)
)


(define (accumulate combiner start n term)
  (if (= n 0)
      start
      (combiner (term n) (accumulate combiner start (- n 1) term))
  )
)


(define (accumulate-tail combiner start n term)
  (cond ((= n 0) start)
        (else (accumulate-tail combiner (combiner start (term n)) (- n 1) term))
  )
)


(define-macro (list-of map-expr for var in lst if filter-expr)
    `(map (lambda (,var) ,map-expr) (filter (lambda (,var) ,filter-expr) ,lst))
)

