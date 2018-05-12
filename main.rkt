#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "classes.rkt")
(require "sudoku_init.rkt")
(require "test_sudokus.rkt")

(define (initialize-candidates! board)
  (define (helper rest-of-elems i)
    (if (> i 81)
        (printf "Sudoku-board set. ~n")
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

(define (set-board! board sdk)
  (let ([elems (send board get-elems)])
    (reset-board! elems)
    (for-each
     (lambda (i)
       (unless (= (list-ref sdk i) 0)
         (send (list-ref elems i) set-value! (list-ref sdk i))
         (send (list-ref elems i) set-user-e! #t)))
     (range 0 81))
    (initialize-candidates! elems)))

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
  (let ([elems (send board get-elems)])
  (unless (sudoku-solved? board) (solve-sudoku! elems)
  (let ((rnd-elem (list-ref elems (random 0 81))))
    (cond
      [(send rnd-elem user-val?)
       (step-solve! elems)]
      [else
       (send rnd-elem set-user-e! #t)
       (clear-filled-elems! elems)
       (initialize-candidates! elems)])))))

(define (solve-sudoku! board)
  (cond
    [(sudoku-solved? board) (printf "Sudoku already solved!")] ;; Only run function if sudoku not yet solved
    [(not (sudoku-solvable? board))
     (printf "Error: sudoku cannot be solved - duplicate elements in row, column and/or box.")
     (clear-filled-elems! (send board get-elems))]
    [else
     (solving-algorithm board)]))
       
(set-board! brd1 sdk1)
(set-board! brd2 sdk2)
(set-board! brd3 sdk3)

;(set-board! brd1 false-sdk1)
