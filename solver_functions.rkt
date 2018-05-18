#lang racket
(provide (all-defined-out))
(require "classes.rkt")
(require "sudoku_init.rkt")

(define (valid-holder? holder) ;; decides if a holder contains any multiple elements or illegal elements (> 9 or < 0)
  (let ([holder-vals (map (lambda (e) (send e get-value))
                          holder)])
    (define (helper vals)
      (cond
        [(null? vals) #t]
        [(= 0 (car vals)) (helper (cdr vals))]
        [(or (> (car vals) 9) (< (car vals) 0) (member (car vals) (cdr vals))) #f]
        [else (helper (cdr vals))]))
    (helper holder-vals)))

(define (sudoku-solvable? board) ;; decides if sudoku has illegal (multiple) elements in a row, col and/or box
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders) #t]
      [(valid-holder? (car rest-of-holders)) (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))

(define (solved-holder? holder) ;; decides if a holder (row, column or box) contains the correct values (1-9 exactly once)
  (let ([elems (map (lambda (e) (send e get-value)) holder)])
    (define (helper vals)
      (cond
        [(null? vals) #t]
        [(not (member (car vals) elems)) #f] ;; when it finds that one of the values 1-9 isn't in the holder, return #f
        [else (helper (cdr vals))]))
    (helper (range 1 10))))

;(define (multiple-solutions? board

(define (sudoku-solved? board)
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders) #t]
      [(solved-holder? (car rest-of-holders))
       (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))

(define (print-board board) ;; prints sudoku board
  (for-each
   (lambda (i)
     (send (list-ref (send board get-elems) i) print-value)
     (when (= (remainder (+ i 1) 9) 0)
       (newline)))
   (range 0 81)))