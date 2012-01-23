#lang racket

(define no-nums
  (lambda (lat)
    (cond [(null? lat) '()]
          [(number? (car lat)) 
           (no-nums (cdr lat))]
          [else (cons (car lat) 
                      (no-nums (cdr lat)))])))

(define rember*
  (lambda (a lat)
    (cond [(null? lat) '()]
          [(list? (car lat)) (cons (rember* a (car lat))
                                   (rember* a (cdr lat)))]
          [(eq? a (car lat)) (rember* a (cdr lat))]
          [else (cons (car lat) (rember* a (cdr lat)))])))

(define occur*
  (lambda (a lat)
    (cond [(null? lat) 0]
          [(list? (car lat)) (+ (occur* a (car lat))
                                (occur* a (cdr lat)))]
          [(eq? a (car lat)) (+ 1 (occur* a (cdr lat)))]
          [else (occur* a (cdr lat))])))

(define leftmost
  (lambda (lat)
    (cond [(list? (car lat)) (leftmost (car lat))]
          [else (car lat)])))

(define eqlist?
  (lambda (l1 l2)
    (cond [(and (null? l1) (null? l2)) #t]
          [(or (null? l1) (null? l2)) #f]
          [(and (list? (car l1)) (list? (car l2))) 
           (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))]
          [(or (list? (car l1)) (list? (car l2))) #f]
          [(eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))]
          [else #f])))