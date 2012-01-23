#lang racket

(define rember
  (lambda (w lat)
    (cond [(null? lat) '()]
          [else (cond [(eq? w (car lat)) (cdr lat)]
                      [else (cons (car lat) (rember w (cdr lat)))])])))

(define rember*
  (lambda (w lat)
    (cond [(null? lat) '()]
          [else (cond [(eq? w (car lat)) (rember* w (cdr lat))]
                      [else (cons (car lat) (rember* w (cdr lat)))])])))

(define rember**
  (lambda (w lat)
    (cond [(null? lat) '()]
          [else (let ([h (car lat)]
                      [t (cdr lat)])
                  (cond [(eq? w h) (rember** w t)]
                        [else (cons h (rember** w t))]))])))