#lang racket
(provide (all-defined-out))

(define sdk1 (list 5 3 0 0 7 0 0 0 0
                   6 0 0 1 9 5 0 0 0
                   0 9 8 0 0 0 0 6 0
                   8 0 0 0 6 0 0 0 3
                   4 0 0 8 0 3 0 0 1
                   7 0 0 0 2 0 0 0 6
                   0 6 0 0 0 0 2 8 0
                   0 0 0 4 1 9 0 0 5
                   0 0 0 0 8 0 0 7 9))

(define sdk2 (list 5 1 2 7 0 0 0 0 0
                   0 7 0 0 4 0 1 0 2
                   8 9 0 0 0 0 0 5 0
                   0 3 0 4 0 2 0 1 0
                   0 0 8 9 6 1 3 0 0
                   0 5 0 8 0 7 0 4 0
                   0 8 0 0 0 0 0 3 9
                   4 0 3 0 9 0 0 2 0
                   0 0 0 0 0 4 8 6 1))

(define sdk3 (list 0 0 0 9 3 0 0 8 7
                   0 0 0 1 0 0 0 4 0
                   4 0 8 0 0 0 1 0 3
                   0 0 0 0 0 7 5 0 9
                   0 0 0 0 0 0 0 0 0
                   3 0 2 5 0 0 0 0 0
                   2 0 3 0 0 0 4 0 1
                   0 5 0 0 0 3 0 0 0
                   6 8 0 0 4 9 0 0 0))

(define sdk4 (list 3 0 0 0 0 0 0 0 8
                   0 0 6 8 0 3 1 0 0
                   0 8 0 0 7 0 0 5 0
                   0 5 0 0 8 0 0 7 0
                   0 0 3 4 0 9 2 0 0
                   0 2 0 0 1 0 0 9 0
                   0 3 0 0 5 0 0 6 0
                   0 0 1 9 0 7 5 0 0
                   9 0 0 0 0 0 0 0 2))