#lang racket
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or ( eq? a (car lat))
                (member? a (cdr lat)))))))
(member? 'b '(c b a))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat))#f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat)(quote()))
      ((member? (car lat) (cdr lat))(makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(makeset '(ej a bo ej blagam ej ej no blagam kamil umyj))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat)(quote()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat)(quote()))
      (else (cons (car lat)
                  (makeset2
                   (multirember (car lat)
                                (cdr lat))))))))

(makeset2 '(ej a bo ej blagam ej ej no blagam kamil umyj))


(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1)#t)
      ((member? (car set1) (set2))(subset? (cdr set1) set2))
      (else #f))))

;subset with and used

(define subsetand?
  (lambda (set1 set2)
    (cond
      ((null? set1)#t)
      (else(and(member? (car set1) set2)(subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and(subset? set1 set2)(subset? set2 set1))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1)#f)
      (else (or (member? (car set1) set2)(intersect (cdr set1) set2))))))

(define retintersect
  (lambda (set1 set2)
    (cond
      ((null? set1)(quote()))
      ((member? (car set1) set2)(cons (car set1) (retintersect (cdr set1) set2)))
      (else (retintersect (cdr set1) set2)))))


(define union
  (lambda (set1 set2)
    (cond
      ((null? set1)set2)
      ((member? (car set1) set2)(union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))


(union '(ciekawe czy to zadziala) '(no normalnie czy to nie wiem zadziala))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set))(car l-set))
      (else (retintersect (car l-set) (intersectall (cdr l-set)))))))

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x)#f)
      ((null? x)#f)
      ((null? (cdr x)#f))
      ((null? (cdr (cdr x)))#t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))
(define second
  (lambda (p)
    (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote())))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))


(define revrel
  (lambda (rel)
    (cond
      ((null? rel)(quote()))
      (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (cdr (car l)) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))
(fullfun? '((bombaj szarakasz) (smierdzi grzybem) (katuj czisa)))
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
