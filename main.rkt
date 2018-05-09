#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "classes.rkt")
(require "sudoku_init.rkt")
(require "test_sudokus.rkt")

(define (initialize-candidates! board)
  (define (helper rest-of-elems i)
    (if (> i 81)
        (printf "Sudoku-board set. ")
        (begin
          (send (car rest-of-elems) update-candidates! #f)
          (helper (cdr rest-of-elems) (+ i 1)))))
  (helper board 1))

(define (reset-board! board)
  (define (helper rest-of-elems i)
    (unless (> i 81)
      (send (car rest-of-elems) set-value! 0)
      (send (car rest-of-elems) reset-all-candidates!)
      (send (car rest-of-elems) set-user-e! #f)
      (helper (cdr rest-of-elems) (+ i 1))))
  (helper board 1))

(define (set-board! board elems)
  (reset-board! board)
  (for-each
   (lambda (i)
     (unless (= (list-ref elems i) 0)
       (send (list-ref board i) set-value! (list-ref elems i))
       (send (list-ref board i) set-user-e! #t)))
   (range 0 81))
  (initialize-candidates! board))

         
(set-board! brd sdk1)
