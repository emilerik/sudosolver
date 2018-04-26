#lang racket
(require "skiss.rkt")

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

(define all-rows (list row1 row2 row3 row4 row5 row6 row7 row8 row9))

(define 123-rows (list row1 row2 row3))

(define 456-rows (list row4 row5 row6))

(define 789-rows (list row7 row8 row9))


(define col1
  (new column%
       [col-num 1]
       [rows all-rows]))

(define col2
  (new column%
       [col-num 2]
       [rows all-rows]))

(define col3
  (new column%
       [col-num 3]
       [rows all-rows]))

(define col4
  (new column%
       [col-num 4]
       [rows all-rows]))

(define col5
  (new column%
       [col-num 5]
       [rows all-rows]))

(define col6
  (new column%
       [col-num 6]
       [rows all-rows]))

(define col7
  (new column%
       [col-num 7]
       [rows all-rows]))

(define col8
  (new column%
       [col-num 8]
       [rows all-rows]))

(define col9
  (new column%
       [col-num 9]
       [rows all-rows]))

(define box1
  (new box%
       [box-num 1]
       [rows 123-rows]
       [cols (list 1 2 3)]))

(define box2
  (new box%
       [box-num 2]
       [rows 123-rows]
       [cols (list 4 5 6)]))

(define box3
  (new box%
       [box-num 3]
       [rows 123-rows]
       [cols (list 7 8 9)]))

(define box4
  (new box%
       [box-num 4]
       [rows 456-rows]
       [cols (list 1 2 3)]))

(define box5
  (new box%
       [box-num 5]
       [rows 456-rows]
       [cols (list 4 5 6)]))

(define box6
  (new box%
       [box-num 6]
       [rows 456-rows]
       [cols (list 7 8 9)]))

(define box7
  (new box%
       [box-num 7]
       [rows 789-rows]
       [cols (list 1 2 3)]))

(define box8
  (new box%
       [box-num 8]
       [rows 789-rows]
       [cols (list 4 5 6)]))

(define box9
  (new box%
       [box-num 9]
       [rows 789-rows]
       [cols (list 7 8 9)]))

(define board
  (new board%
       [rows all-rows]))

       
