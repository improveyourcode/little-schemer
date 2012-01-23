#lang racket

(define insertR
  (lambda (old new xs)
    (cond [(null? xs) '()]
          [(eq? old (car xs)) (cons (car xs) (cons new (cdr xs)))]
          [else (cons (car xs) (insertR old new (cdr xs)))])))

(define insertL
  (lambda (old new xs)
    (cond [(null? xs) '()]
          [(eq? old (car xs)) (cons new xs)]
          [else (cons (car xs) (insertL old new (cdr xs)))])))

(define subst
  (lambda (old new lat)
    (cond [(null? lat) '()]
          [(eq? old (car lat)) (cons new (cdr lat))]
          [else (cons (car lat) (subst old new (cdr lat)))])))

