;; This file contains all the solving algorithms, and the help procedures to check if a board is solved or solvable. 
;; It has the master solve-function and its slaves, solving-algorithm and solve-singletons. It also has a step solve-procedure.
;; This file can be used as a backdoor to set a board from a list of a sudoku, solving it, and printing it, without using the graphical interface.
;; Authors: Algirdas Bartkevicius & Emil Eriksson
;; Last update: 2018-05-23
;; Moved some functions to board_functions.rkt

#lang racket
(provide (all-defined-out))
(require "board_functions.rkt")

;; Global iteration counter
(define iter 0)

;; I/O: board% / boolean
;; Decides if a holder (row, column or box) contains the correct values (1-9 exactly once).
(define (solved-holder? holder) 
  (let ([elems (map (lambda (e) (send e get-value)) holder)])
    (define (helper vals)
      (cond
        [(null? vals) #t]
        [(not (member (car vals) elems)) #f] ;; When it finds that one of the values 1-9 isn't in the holder, return #f
        [else (helper (cdr vals))]))
    (helper (range 1 10))))

;; I/O: board% / boolean
;; Calls if all 27 holders (9 columns, 9 rows and 9 boxes) has values 1-9 exactly once. The main rule of Sudoku.
(define (sudoku-solved? board)
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders) #t]
      [(solved-holder? (car rest-of-holders))
       (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))

;; I/O: holder = list of element% / boolean
;; Decides if a holder contains any multiple elements or illegal elements (> 9 or < 0).
(define (valid-holder? holder) 
  (let ([holder-vals (map (lambda (e) (send e get-value))
                          holder)])
    (define (helper vals)
      (cond
        [(null? vals)]
        [(not (and (number? (car vals)) (> (car vals) -1) (< (car vals) 10)))
         (printf "Error: sudoku contains illegal elements. Valid values are 1-9.~n")
         #f]
        [(= 0 (car vals)) (helper (cdr vals))]
        [(member (car vals) (cdr vals))
         (printf "Error: duplicate elements in row, column and/or box.~n")
         #f]
        [else (helper (cdr vals))]))
    (helper holder-vals)))

;; I/O: board% / boolean
;; Decides if sudoku has illegal (multiple) elements in a row, col and/or box.
(define (sudoku-solvable? board) 
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders)]
      [(valid-holder? (car rest-of-holders)) (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))

;; I/O: board% / No output
;; Master function for calling solving procedures
(define (solve-sudoku! board)
  (cond
    [(sudoku-solved? board) (printf "Sudoku already solved!")] ;; Only run function if sudoku not yet solved.
    [(not (sudoku-solvable? board)) ;; Illegal sudoku.
     (printf "Sudoku cannot be solved.")] 
    [else
     (let ([time (current-inexact-milliseconds)])
       (initialize-candidates! (send board get-elems)) ;; Makes candidate list and updates them by checking friends values.
       (solve-singletons! (send board get-elems))  ;; Try solving fully or partly by finding elems with single candidates.
       (unless (sudoku-solved? board)
         (solving-algorithm! board))
       (set! time (- (current-inexact-milliseconds) time))
       (printf "Sudoku solved! ~nNumber of iterations: ~a.~nTime: ~a ms.~n~n" iter time)
       (print-board board)
       (set! iter 0))]))
  

;; I/O: list of element% / No output
;; Finds all elements who has only one possible candidate and makes it his value. Also eliminates such a element from the friends list.
(define (solve-singletons! elems)
  (set! iter (+ iter 1)) ;; iteration counter
  (let ([found-singleton #f])
    (for-each (lambda (e)
                (when (= 1 (length (send e get-candidates)))
                  (set! found-singleton #t)
                  (send e set-cand-to-val!)
                  (send e set-user-e! #t)
                  (rm-cand-from-friends! (send e get-friends) (send e get-value))))
              elems)
    (when found-singleton (solve-singletons! elems))))

;; I/O: board% / No output
;; Solves sudoku board by going forward or backward in elements list.
(define (solving-algorithm! board)
  (let ([first-e (car (send board get-elems))])
    (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
      (set! iter (+ iter 1)) ;; iteration counter
      (cond
        [(equal? (send curr-e get-value) 'last)] ;; Made it to the last element.
         
        [(send curr-e user-val?)
         (cond
           [(or (eqv? prev-e 'first) f)
            (send next-e update-candidates! #t)
            (helper curr-e next-e (send next-e get-next-e) #t)]
           [else
            (helper (send prev-e get-prev-e) prev-e curr-e #f)])]
        
        [(send curr-e empty-cand?)
         (send curr-e set-value! 0)
         (send curr-e reset-candidates!)
         (helper (send prev-e get-prev-e) prev-e curr-e #f)]
        
        [else
         (send curr-e set-cand-to-val!)
         (send next-e update-candidates! #t)
         (helper curr-e next-e (send next-e get-next-e) #t)]))
    (helper 'first first-e (send first-e get-next-e) #t)))

;; I/O: board% / No output
;; Solves one random value on the board.
(define (step-solve! board)
  (if (sudoku-solved? board)
      (printf "Sudoku already solved!~n")
      (begin 
        (step-solve-helper board)
        (print-board board))))

(define (step-solve-helper board)
  (solving-algorithm! board)
  (set! iter 0)
  (let* ([elems (send board get-elems)] [rnd-elem (list-ref elems (random 0 81))])
    (cond
      [(send rnd-elem user-val?)
       (clear-filled-elems! elems)
       (step-solve-helper board)]
      [else
       (send rnd-elem set-user-e! #t)
       (clear-filled-elems! elems)])))

