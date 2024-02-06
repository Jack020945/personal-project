(define (rle s)
    
    (define (extract-run s prev count)
        (cond
            ((null? prev) (extract-run (cdr-stream s) (car s) 1))
            ((null? s) (list prev count))
            ((= (car s) prev) (extract-run (cdr-stream s) (car s) (+ count 1)))
            (else (list prev count))
        )
    )
    
    (define (trim s prev)
        (cond
            ((null? prev) (trim (cdr-stream s) (car s)))
            ((null? s) nil)
            ((= (car s) prev) (trim (cdr-stream s) (car s)))
            (else s)
        )
    )
    
    (if (null? s)
        nil
        (cons-stream (extract-run s nil 0) (rle (trim s nil)))
    )
)



(define (group-by-nondecreasing s)
    
    (define (extract-increasing s prev lst)
        (cond
            ((null? prev) (extract-increasing (cdr-stream s) (car s) (list (car s))))
            ((null? s) lst)
            ((> (car s) prev) (extract-increasing (cdr-stream s) (car s) (append lst (list (car s)))))
            ((= (car s) prev) (extract-increasing (cdr-stream s) (car s) (append lst (list (car s)))))
            (else lst)
        )
    )
    
    (define (trim s prev)
        (cond
            ((null? prev) (trim (cdr-stream s) (car s)))
            ((null? s) nil)
            ((> (car s) prev) (trim (cdr-stream s) (car s)))
            ((= (car s) prev) (trim (cdr-stream s) (car s)))
            (else s)
        )
    )
    
    (if (null? s)
        nil
        (cons-stream (extract-increasing s nil nil) (group-by-nondecreasing (trim s nil)))
    )
)


(define finite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 3
                (cons-stream 1
                    (cons-stream 2
                        (cons-stream 2
                            (cons-stream 1 nil))))))))

(define infinite-test-stream
    (cons-stream 1
        (cons-stream 2
            (cons-stream 2
                infinite-test-stream))))

