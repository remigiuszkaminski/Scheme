#lang racket

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(firsts '(((a) b) (c d) (e f)))

(define insertR
  (lambda (old new lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR old new (cdr lat)))))))

(insertR 'd 'e '(a b c d f g d h))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(insertL 'e 'd '(a b c d f g d h))


(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(subst 'e 'd '(a b c d f g d h))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat)(quote()))
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(subst2 'nowe 'f 'd '(a b c d f g d h))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)(quote()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(multirember 'a '(b c a d a d))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(multiinsertR 'e 'd '(a b c d f g d h))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new (cons (car lat) (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))
(multiinsertL 'e 'd '(a b c d f g d h))

(define multisubst
  (lambda (old new lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat))(cons new (multisubst old new (cdr lat))))
      (else (cons (car lat) (multisubst old new (cdr lat)))))))

(multisubst 'a 'b '(a c a c a a c c a))

