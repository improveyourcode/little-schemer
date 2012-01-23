#lang racket

(define firsts 
  (lambda (xss)
    (cond [(null? xss) '()]
          [else (cons (caar xss) (firsts (cdr xss)))])))

(define firsts*
  (lambda (xss)
    (cond [(null? xss) '()]
          [(null? (car xss)) '()]
          [else (cons (caar xss) (firsts* (cdr xss)))])))