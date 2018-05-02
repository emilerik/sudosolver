#lang racket
(require "classes.rkt")
(provide (all-defined-out))

(define e81
  (new element%))
(define e80
  (new element%
       [next-e e81]))
(define e79
  (new element%
       [next-e e80]))
(define e78
  (new element%
       [next-e e79]))
(define e77
  (new element%
       [next-e e78]))
(define e76
  (new element%
       [next-e e77]))
(define e75
  (new element%
       [next-e e76]))
(define e74
  (new element%
       [next-e e75]))
(define e73
  (new element%
       [next-e e74]))
(define e72
  (new element%
       [next-e e73]))
(define e71
  (new element%
       [next-e e72]))
(define e70
  (new element%
       [next-e e71]))
(define e69
  (new element%
       [next-e e70]))
(define e68
  (new element%
       [next-e e69]))
(define e67
  (new element%
       [next-e e68]))
(define e66
  (new element%
       [next-e e67]))
(define e65
  (new element%
       [next-e e66]))
(define e64
  (new element%
       [next-e e65]))
(define e63
  (new element%
       [next-e e64]))
(define e62
  (new element%
       [next-e e63]))
(define e61
  (new element%
       [next-e e62]))
(define e60
  (new element%
       [next-e e61]))
(define e59
  (new element%
       [next-e e60]))
(define e58
  (new element%
       [next-e e59]))
(define e57
  (new element%
       [next-e e58]))
(define e56
  (new element%
       [next-e e57]))
(define e55
  (new element%
       [next-e e56]))
(define e54
  (new element%
       [next-e e55]))
(define e53
  (new element%
       [next-e e54]))
(define e52
  (new element%
       [next-e e53]))
(define e51
  (new element%
       [next-e e52]))
(define e50
  (new element%
       [next-e e51]))
(define e49
  (new element%
       [next-e e50]))
(define e48
  (new element%
       [next-e e49]))
(define e47
  (new element%
       [next-e e48]))
(define e46
  (new element%
       [next-e e47]))
(define e45
  (new element%
       [next-e e46]))
(define e44
  (new element%
       [next-e e45]))
(define e43
  (new element%
       [next-e e44]))
(define e42
  (new element%
       [next-e e43]))
(define e41
  (new element%
       [next-e e42]))
(define e40
  (new element%
       [next-e e41]))
(define e39
  (new element%
       [next-e e40]))
(define e38
  (new element%
       [next-e e39]))
(define e37
  (new element%
       [next-e e38]))
(define e36
  (new element%
       [next-e e37]))
(define e35
  (new element%
       [next-e e36]))
(define e34
  (new element%
       [next-e e35]))
(define e33
  (new element%
       [next-e e34]))
(define e32
  (new element%
       [next-e e33]))
(define e31
  (new element%
       [next-e e32]))
(define e30
  (new element%
       [next-e e31]))
(define e29
  (new element%
       [next-e e30]))
(define e28
  (new element%
       [next-e e29]))
(define e27
  (new element%
       [next-e e28]))
(define e26
  (new element%
       [next-e e27]))
(define e25
  (new element%
       [next-e e26]))
(define e24
  (new element%
       [next-e e25]))
(define e23
  (new element%
       [next-e e24]))
(define e22
  (new element%
       [next-e e23]))
(define e21
  (new element%
       [next-e e22]))
(define e20
  (new element%
       [next-e e21]))
(define e19
  (new element%
       [next-e e20]))
(define e18
  (new element%
       [next-e e19]))
(define e17
  (new element%
       [next-e e18]))
(define e16
  (new element%
       [next-e e17]))
(define e15
  (new element%
       [next-e e16]))
(define e14
  (new element%
       [next-e e15]))
(define e13
  (new element%
       [next-e e14]))
(define e12
  (new element%
       [next-e e13]))
(define e11
  (new element%
       [next-e e12]))
(define e10
  (new element%
       [next-e e11]))
(define e9
  (new element%
       [next-e e10]))
(define e8
  (new element%
       [next-e e9]))
(define e7
  (new element%
       [next-e e8]))
(define e6
  (new element%
       [next-e e7]))
(define e5
  (new element%
       [next-e e6]))
(define e4
  (new element%
       [next-e e5]))
(define e3
  (new element%
       [next-e e4]))
(define e2
  (new element%
       [next-e e3]))
(define e1
  (new element%
       [next-e e2]))


(define row1
  (new row%
       [row-number 1]
       [elements (list e1 e2 e3 e4 e5 e6 e7 e8 e9)]))

(define row2
  (new row%
       [row-number 2]
       [elements (list e10 e11 e12 e13 e14 e15 e16 e17 e18)]))

(define row3
  (new row%
       [row-number 3]
       [elements (list e19 e20 e21 e22 e23 e24 e25 e26 e27)]))

(define row4
  (new row%
       [row-number 4]
       [elements (list e28 e29 e30 e31 e32 e33 e34 e35 e36)]))

(define row5
  (new row%
       [row-number 5]
       [elements (list e37 e38 e39 e40 e41 e42 e43 e44 e45)]))

(define row6
  (new row%
       [row-number 6]
       [elements (list e46 e47 e48 e49 e50 e51 e52 e53 e54)]))

(define row7
  (new row%
       [row-number 7]
       [elements (list e55 e56 e57 e58 e59 e60 e61 e62 e63)]))

(define row8
  (new row%
       [row-number 8]
       [elements (list e64 e65 e66 e67 e68 e69 e70 e71 e72)]))

(define row9
  (new row%
       [row-number 9]
       [elements (list e73 e74 e75 e76 e77 e78 e79 e80 e81)]))

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



       
