;; This file contains the procedures for interacting with the board%. For example, initializing candidates, resetting, setting and printing board.
;; Authors: Algirdas Bartkevicius & Emil Eriksson
;; Last update: 2018-05-23
;; Created file, moved procedures from solver_functions.rkt

#lang racket
(provide (all-defined-out))
(require "element.rkt")
(require "board.rkt")
(require "test_sudokus.rkt")
(require "sudoku_init.rkt")

;; I/O: list of element% / No output
;; Resets the board by resetting all elements, candidates and init-candidates
(define (reset-board! elems)
  (define (helper rest-of-elems i)
    (unless (> i 81)
      (send (car rest-of-elems) set-value! 0)
      (send (car rest-of-elems) reset-all-candidates!)
      (send (car rest-of-elems) set-user-e! #f)
      (helper (cdr rest-of-elems) (+ i 1))))
  (helper elems 1))

;; I/O: board%, list of sudoku values / No output
;; Sets a sudoku board to contain the values from a list
(define (set-board! board sdk)
  (let ([elems (send board get-elems)])
    (reset-board! elems)
    (for-each
     (lambda (i)
       (unless (= (list-ref sdk i) 0)
         (send (list-ref elems i) set-value! (list-ref sdk i))
         (send (list-ref elems i) set-user-e! #t)))
     (range 0 81))
    (initialize-candidates! (send board get-elems))))

;; I/O: friends of element%, candidate value / No output
;; Goes through an element%'s friends, and removes a value from their candidate list.
(define (rm-cand-from-friends! friends candidate)
  (for-each (lambda (e)
     (send e rm-candidate! candidate))
   friends))

;; I/O: list of element% / No output
;; Clears all values that aren't flagged as user-values.
(define (clear-filled-elems! elems) 
  (define (helper elems)
    (cond
      [(null? elems) (void)]
      [(send (car elems) user-val?)
       (helper (cdr elems))]
      [else
       (send (car elems) set-value! 0)
       (send (car elems) reset-all-candidates!)
       (helper (cdr elems))]))
  (helper elems))

;; I/O: list of element% / No output
;;Checks all elements and ,if needed, updates their candidate list.
(define (initialize-candidates! elems)
  (define (helper rest-of-elems i)
    (unless (or (> i 81) (null? rest-of-elems))
      (send (car rest-of-elems) update-candidates! #f)
      (helper (cdr rest-of-elems) (+ i 1))))
  (helper elems 1))

;; I/O board% / list of elements values
;; Prints the element values of a board.
(define (print-board board) 
  (for-each
   (lambda (i)
     (send (list-ref (send board get-elems) i) print-value)
     (when (= (remainder (+ i 1) 9) 0)
       (newline)))
   (range 0 81)))
                 
(define brd1 (make-board))
(define brd2 (make-board))
(define brd3 (make-board))
(define brd4 (make-board))
(define false-brd1 (make-board))
       
(set-board! brd1 sdk1)
(set-board! brd2 sdk2)
(set-board! brd3 sdk3)
(set-board! brd4 sdk4)
(set-board! false-brd1 false-sdk1)