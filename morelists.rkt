#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l)
                     (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(rember* 'didi '((elo didi) didi (ej (kej didi)) benc))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons (car l) (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'czisa 'katuj '(katuj (prosze (kamil katuj mi)) (kamil (kamil (kamil (kamil katuj mi)))) katuj))
;expected output: '(katuj czisa (prosze (kamil katuj czisa mi)) (kamil (kamil (kamil (kamil katuj czisa mi)))) katuj czisa)
;output: '(katuj czisa (prosze (kamil katuj czisa mi)) (kamil (kamil (kamil (kamil katuj czisa mi)))) katuj czisa)


(define occur*
  (lambda (a l)
    (cond
      ((null? l)0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (+ (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'myjmi '((myjmi) (split ((((myjmi ice))) (cream (myjmi)) sherbet)) (myjmi) (bread) (myjmi brandy)))



(define subst*
  (lambda (new old l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'tymi 'sobie '(sobie (ty (tymi a on sobie)) (ej (no (prosze bardzo (zeby sobie katis))))))

(define insertL*
  (lambda (old new l)
    (cond
      ((null? l)(quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons new (cons old (insertL* old new (cdr l)))))
         (else (cons (car l) (insertL* old new (cdr l))))))
      (else (cons (insertL* old new (car l)) (insertL* old new (cdr l)))))))

(insertL* 'katuj 'czisa '(katuj (prosze (kamil katuj mi)) (kamil (kamil (kamil (kamil katuj mi)))) katuj))

(define member*
  (lambda (a l)
    (cond
      ((null? l)#f)
      ((atom? (car l))
       (or (eq? (car l) a)
        (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))


(member* 'supertunia '(kwiaty lubie (masne sa (fest (nie (supertunia to w ogole))))))


(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(leftmost '((((elo))) e a))

(define eqan?
  (lambda (n m)
    (cond
      ((and (number? n) (number? m)) (= n m))
      ((or (number? n) (number? m))#f)
      (else (eq? n m)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1)(null? l2))#t)
      ((or (null? l1)(null? l2))#f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (eqan? (car l1) (car l2))
       (eqlist? (cdr l1) (cdr l2)))
      ((or (atom? (car l1)) (atom? (car l2)))#f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(eqlist? '((majo) bajo (elo (jajo))) '((majo) bajo (elo (jajo))))


(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((atom? s1)#f)
      ((atom? s2)#f)
      (else (eqlist2? s1 s2)))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))#t)
      ((or (null? l1) (null? l2))#f)
      (else (and (equal? (car l1) (car l2)) (eqlist2? (cdr l1) (cdr l2)))))))

(eqlist2? '(no i) '(no i))


