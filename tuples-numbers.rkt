#lang racket

(define add1
  (lambda (n)
    (+ 1 n)))

(define sub1
  (lambda (n)
    (- n 1)))

(define sum
  (lambda (n m)
    (cond [(zero? m) n]
          [else (add1 (sum n (sub1 m)))])))

(define sub
  (lambda (n m)
    (cond [(zero? m) n]
          [else (sub1 (sub n (sub1 m)))])))

(define addtup
  (lambda (tup)
    (cond [(null? tup) 0]
          [else (sum (car tup) (addtup (cdr tup)))])))

(define mul
  (lambda (n m)
    (cond [(zero? m) 0]
          [else (sum n (mul n (sub1 m)))])))

(define tup+
  (lambda (tup1 tup2)
    (cond [(or (null? tup1) (null? tup2)) '()]
          [else (cons (sum (car tup1) (car tup2))
                      (tup+ (cdr tup1) (cdr tup2)))])))

(define ^
  (lambda (n m)
    (cond [(zero? m) 1]
          [else (mul n (^ n (sub1 m)))])))

(define pick
  (lambda (k lat)
    (cond [(zero? (sub1 k)) (car lat)]
          [else (pick (sub1 k) (cdr lat))])))

(define rempick
  (lambda (k lat)
    (cond [(zero? (sub1 k)) (cdr lat)]
          [else (cons (car lat) (rempick (sub1 k) (cdr lat)))])))