#lang racket
(provide (all-defined-out))

(define row1
  (new row%
       [row-number 1]))

(define row2
  (new row%
       [row-number 2]))

(define row3
  (new row%
       [row-number 3]))

(define row4
  (new row%
       [row-number 4]))

(define row5
  (new row%
       [row-number 5]))

(define row6
  (new row%
       [row-number 6]))

(define row7
  (new row%
       [row-number 7]))

(define row8
  (new row%
       [row-number 8]))

(define row9
  (new row%
       [row-number 9]))


(define col2
  (new column%
       [column-number 2]))

(define col5
  (new column%
       [column-number 9]))

(hash-set! *elements* 1 (send row1 get-element column-number))
(hash-set! *elements* 2 (send row2 get-element column-number))
(hash-set! *elements* 3 (send row3 get-element column-number))
(hash-set! *elements* 4 (send row4 get-element column-number))
(hash-set! *elements* 5 (send row5 get-element column-number))
(hash-set! *elements* 6 (send row6 get-element column-number))
(hash-set! *elements* 7 (send row7 get-element column-number))
(hash-set! *elements* 8 (send row8 get-element column-number))
(hash-set! *elements* 9 (send row9 get-element column-number))


       
