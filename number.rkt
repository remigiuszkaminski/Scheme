#lang racket

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(zero? 0)

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else ( add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else ( sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup)0)
      (else (+ (car tup)(addtup (cdr tup)))))))

(addtup '(1 2 3))



(define *
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (+ n (* n (sub1 m)))))))

(* 2 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1)(car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(1 2 3))

(define >
  (lambda (n m)
    (cond
      ((zero? n)#f)
      ((zero? m)#t)
      (else (> (sub1 n) (sub1 m))))))

(> 3 4)

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))


(define n=
  (lambda (n m)
    (cond
      ((> n m)#f)
      ((< n m)#f)
      (else #t))))
(define pow
  (lambda (n m)
    (cond
      ((zero? m)1)
      (else (* n (pow n (sub1 m)))))))

(pow '5 '3)

(define div
  (lambda (n m)
    (cond
      ((< n m)0)
      (else (add1 (div (- n m) m))))))

(div '15 '3)

(define length
  (lambda (lat)
    (cond
      ((null? lat)0)
      (else (add1 (length (cdr lat)))))))

(length '(1 2 3 4 5 6))

(define pick
  (lambda (num lat)
    (cond
      ((zero? (sub1 num))(car lat))
      (else (pick (sub1 num)(cdr lat))))))

(pick '2 '(dzida dzidson dziddd))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n))(cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick '3 '(a b c d))

(number? '2)

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat)(quote ()))
      ((number? (car lat))(no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(1 2 elo bajlando 4 x))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat))(cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(1 e 2 3 dsds x d 4 5))

(define eqan?
  (lambda (n m)
    (cond
      ((and (number? n) (number? m)) (= n m))
      ((or (number? n) (number? m))#f)
      (else (eq? n m)))))

(eqan? '3 '3)

(define occur
  (lambda (n lat)
    (cond
      ((null? lat)0)
      ((eqan? n (car lat))(add1 (occur n (cdr lat))))
      (else (occur n (cdr lat))))))

(occur 'a '(b c d a e f a d a))

(define one?
  (lambda (n)
    (cond
      ((zero? n) #f)
      (else (zero? (sub1 n))))))

(one? '1)

(one? '2)
(define rempick2
  (lambda (n lat)
    (cond
      ((one? n)(cdr lat))
      (else (cons (car lat) (rempick2 (sub1 n) (cdr lat)))))))
(rempick2 '3 '(won z mojego pola gnoje))

