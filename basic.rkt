#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(atom? (quote ()))

(quote atom)
(atom? (quote atom))

(atom? 'turkey)
(quote (atom))
(quote (atom turkey or))

(define l '((a b (c))))
(car l)

(define s 'a)
s

(define x '((b) c d))

(cons s (car x))


(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #t))))

(lat? '(a b c))


(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or ( eq? a (car lat))
                (member? a (cdr lat)))))))
(member? 'b '(c b a))