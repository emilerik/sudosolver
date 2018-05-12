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

(define (clear-filled-elems! board) ;; Clears all values that aren't flagged as user-values
  (cond
    [(null? board)
     (void)]
    [(send (car board) user-val?)
     (clear-filled-elems! (cdr board))]
    [else
     (send (car board) set-value! 0)
     (send (car board) reset-all-candidates!)
     (clear-filled-elems! (cdr board))]))

(define (step-solve! board)
  (unless (sudoku-solved?) (solve-sudoku! board))
  (let ((rnd-elem (list-ref board (random 0 81))))
    (cond
      [(send rnd-elem user-val?)
       (step-solve! board)]
      [else
       (send rnd-elem set-user-e! #t)
       (clear-filled-elems! board)
       (initialize-candidates! board)])))

(define (solve-sudoku! board)
  (cond
    [(sudoku-solved?) (printf "Sudoku already solved!")] ;; Only run function if sudoku not yet solved
    [(not (sudoku-solvable?))
     (printf "Error: sudoku cannot be solved - duplicate elements in row, column and/or box.")
     (clear-filled-elems! board)]
    [else
     (solving-algorithm board)]))
       
(set-board! brd sdk1)
;(set-board! brd false-sdk1)

(define rezz 0)
(set! rezz brd)
