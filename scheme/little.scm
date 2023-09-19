;; code in The Little Schemer, 4th edition
;; using a racket tool set, but scheme

#lang scheme

;;
;; scheme equals:
;;
;; Remarks:
;; ---------
;;   I want to remark that equal is a strange concept, especially when it is
;;   associated with equations. Actually, equation is rarely fully equal.
;;
;;   when we write a equation like below in first grade:
;;   1 + 1 = 2
;;   we are saying 1 + 1 equals to 2, but how? apparently, the left hand side is
;;   three characters(five if you count spaces), and the right hand side is only
;;   one. They are different.
;;
;;   But why we write this symbol? We do mean that they are the same in some aspects.
;;   So equal are not same. Every equal represents that they are same in some context,
;;   but not in others.
;;
;;   Surely if '=' means fully equal, there will be no reason for equations to exists.
;;   What can you infer from equations like '1=1' ?
;;
;;   So it's nature to have different kinds of equals in scheme as well.
;;
;; = : number equal
;; eq?: memory address equal
;; eqv?: like eq?, but for primitive values compare values
;; equal?: test equal by value
;;

(define member?
  (lambda (c mlist)
    (cond
      ((null? mlist) #f)
      ((eq? c (car mlist)) #t)
      (else (member? c (cdr mlist))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat))
       #f)
      (else
       (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat)
       (quote ()))
      ((member? (car lat) (cdr lat))
       (makeset (cdr lat)))
      (else
       (cons (car lat) (makeset (cdr lat)))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1)
       #t)
      (else
       (and (member? (car set1) set2)
            (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1)
       #f)
      (else
       (or (member? (car set1) set2)
           (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1)
       '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
       (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1)
       set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else
       (cons (car set1) (union (cdr set1) set2))))))

(define atom?
  (lambda (x)
    (not (or (pair? x) (null? x)))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define (pair-first p) (car p))

(define (pair-second p) (car (cdr p)))

(define (pair-build s1 s2) (cons s1 (cons s2 '())))

(define (firsts lst)
  (cond
    ((null? lst) '())
    ((null? (car lst)) '())
    (else
     (cons (car (car lst)) (firsts (cdr lst))))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define (revrel rel)
  (cond
    ((null? rel) '())
    (else
     (cons (pair-build
            (pair-second (car rel))
            (pair-first (car rel)))
           (revrel (cdr rel))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a)
         (cdr l))
        (else
         (cons (car l) (rember-f test? a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define (insert-g seq)
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((eq? (car l) old)
       (seq new old (cdr l)))
      (else
       (cons (car l)
             (insert-g seq) new old (cdr l))))))

(define substitute (insert-g (lambda (new _ l) (cons new l))))

(define (multiremberT test? lat)
  (cond
    ((null? lat) '())
    ((test? (car lat))
     (multiremberT test? (cdr lat)))
    (else
     (cons (car lat)
           (multiremberT test? (cdr lat))))))

;; looks at every atom of the lat to see whether it's eq? to a. Those atoms that are not are collected
;; in one list ls1; the others for which the answer is true are collected in a second list ls2.
;; Finally, it determines the value of (f ls1 ls2)
(define (multirember&co a lat col)
  (cond
    ((null? lat)
     (col '() '()))
    ((eq? (car lat) a)
     (multirember&co a
                     (cdr lat)
                     (lambda (newlat seen)
                       (col newlat
                            (cons (car lat) seen)))))
    (else
     (multirember&co a
                     (cdr lat)
                     (lambda (newlat seen)
                       (col (cons (car lat) newlat) seen))))))

(define (evens-only*&co l col)
  (cond
    ((null? l)
     (col '() 1 0))
    ((atom? (car l))
     (cond
       ((even? (car l))
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col (cons (car l) newl) (* (car l) p) s))))
       (else
        (evens-only*&co (cdr l)
                        (lambda (newl p s)
                          (col newl p (+ (car l) s)))))))
    (else
     (evens-only*&co (car l)
                     (lambda (al ap as)
                       (evens-only*&co (cdr l)
                                       (lambda (dl dp ds)
                                         (col (cons al dl) (* ap dp) (+ as ds)))))))))

(define (the-last-friend newl product sum)
  (cons sum (cons product newl)))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

;; ch09

(define looking
  (lambda (a lat) (keep-looking a (pick 1 lat) lat)))

(define (pick n lat)
  (cond
    ((= n 1) (car lat))
    (else
     (pick (- n 1) (cdr lat)))))

(define (keep-looking a sorn lat)
  (cond
    ((number? sorn)
     (keep-looking a (pick sorn lat) lat))
    (else
     (equal? sorn a))))

(looking 'caviar '(6 2 4 cavier 5 7 3))

;; skip ch09 now
;; ch10

(define new-entry pair-build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-helper name (first entry) (second entry) entry-f))

(define (lookup-in-entry-helper name names values entry-f)
  (cond
    (null? names) (entry-f name)
    (eq? (car names) name) (car values)
    (else
     (lookup-in-entry-helper name
                             (cdr names)
                             (cdr values)
                             entry-f))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond
    ((null? table)
     (table-f name))
    (else
     (lookup-in-entry name (car table)
                      (lambda (name) (lookup-in-table name (cdr table) table-f))))))

(define (expression-to-action e)
  (cond
    ((atom? e) (atom-to-action e))
    (else
     (list-to-action e))))

(define (atom-to-action e)
  (cond
    ((number? e) *const)
    ((eqv? e #t) *const)
    ((eqv? e #f) *const)
    ((eqv? e 'cons) *const)
    ((eqv? e 'car) *const)
    ((eqv? e 'cdr) *const)
    ((eqv? e 'null?) *const)
    ((eqv? e 'eq?) *const)
    ((eqv? e 'atom?) *const)
    ((eqv? e 'zero?) *const)
    ((eqv? e 'add1) *const)
    ((eqv? e 'sub1 *const))
    ((eqv? e 'number?) *const)
    (else *identifier)))


(define (list-to-action e)
  (cond
    ((atom? (car e))
     (cond
       ((eq? (car e) 'quote) *quote)
       ((eq? (car e) '\') *quote)
       ((eq? (car e) 'lambda) *lambda)
       ((eq? (car e) 'cond) *cond)
       (else *application)))
    (else *application)))

(define (value e) (meaning e '()))

(define (meaning e table) ((expression-to-action e) e table))

(define (*const e table)
  (cond
    ((number? e) e)
    ((eqv? e #t) #t)
    ((eqv? e #f) #f)
    (else
     (pair-build 'primitive e))))

(define (*quote e table) (text-of e))

(define text-of second)

(define (*identifier e table) (lookup-in-table e table initial-table))

(define (initial-table name) (car '(())))

(define (*lambda e table)
  (pair-build 'non-primitive (cons table (cdr e))))

(define table-of first)

(define formals-of second)

(define body-of third)

(define (evcon lines table)
  (cond
    ((else? (question-of (car lines)))
     (meaning (answer-of (car lines) table)))
    ((meaning (question-of (car lines)) table)
     (meaning (answer-of (car lines) table)))
    (else (evcon (cdr lines) table))))

(define (else? x)
  (cond
    ((atom? x) (eqv? x 'else))
    (else #f)))

(define question-of first)

(define answer-of second)

(define (*cond e table) (evcon (cond-lines-of e) table))

(define cond-lines-of cdr)

(define (evlis args table)
  (cond
    ((null? args) '())
    (else
     (cons (meaning (car args) table)
           (evlis (cdr args) table)))))

(define (*application e table)
  (apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define function-of car)
(define arguments-of cdr)

(define (primitive? l) (eqv? (first l) 'primitive))

(define (non-primitive? l) (eqv? (first l) 'non-primitive))

(define (apply fun vals)
  (cond
    ((primitive? fun)
     (apply-primitive (second fun) vals))
    ((non-primitive? fun)
     (apply-closure (second fun) vals))))

(define (apply-primitive name vals)
  (cond
    ((eqv? name 'cons)
     (cons (first vals) (second vals)))
    ((eqv? name 'car)
     (car (first vals)))
    ((eqv? name 'cdr)
     (cdr (first vals)))
    ((eqv? name 'null?)
     (null? (first vals)))
    ((eqv? name 'eq?)
     (eqv? (first vals) (second vals)))
    ((eqv? name 'atom?)
     (:atom? (first vals)))
    ((eqv? name 'zero?)
     (zero? (first vals)))
    ((eqv? name 'add1)
     (add1 (first vals)))
    ((eqv? name 'sub1)
     (sub1 (first vals)))
    ((eqv? name 'number?)
     (number? (first vals)))))

(define (:atom? x)
  (cond
    ((atom? x) #t)
    ((null? x) #f)
    ((eqv? (car x) 'primitive) #t)
    ((eqv? (car x) 'non-primitive) #t)))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry formals-of closure)
                         vals)
           (table-of closure)))
