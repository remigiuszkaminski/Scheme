#lang racket

(define (teste n)
  (cond((< n 3) n)
       (else (+ (teste(- n 1))(* 2 (teste(- n 2)))(* 3 (teste(- n 3)))))))

(teste 20)
(define (compute-recursive n)
  (cond
    ((< n 3) n)
    (else (+ (compute-recursive (- n 1))
             (* 2 (compute-recursive (- n 2)))
             (* 3 (compute-recursive (- n 3)))))))
(compute-recursive 20)

(define (pascal-row n)
  (cond
    ((= n 0) '(1))
    (else (next-row (pascal-row (- n 1))))))

(define (next-row row)
  (cons 1 (add-adjacent row)))

(define (add-adjacent row)
  (if (null? (cdr row))
      '(1)
      (cons (+ (car row) (cadr row))
            (add-adjacent (cdr row)))))

(pascal-row 4)