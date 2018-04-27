#lang racket
(provide (all-defined-out))
(require "classes.rkt")
(require "world_init.rkt")

;(define/public (sudoku-solved? sdk)
;  ())

(define (sudoku-solve sdk)
  (define (helper result rest-of-sdk prev-element)
    (if (null? rest-of-sdk)
        (reverse result)
        (let ([first-ele (first rest-of-sdk)])
          (cond
            [(null? rest-of-sdk) result]
            [(number? first-ele)
             (helper (cons first-ele result) (cdr rest-of-sdk))]
            [else
             (helper (cons (car first-ele) result) (cdr rest-of-sdk))]))))
  (helper '() sdk))

(define (board-to-list board)
  (string-join board))