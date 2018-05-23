;; This file contains all the solving algorithms, and the help procedures to check if a board is solved or solvable. 
;; It has the master solve-function and its slaves, solving-algorithm and solve-singletons. It also has a step solve-procedure.
;; This file can be used as a backdoor to set a board from a list of a sudoku, solving it, and printing it, without using the graphical interface.
;; Authors: Algirdas Bartkevicius & Emil Eriksson
;; Last update: 2018-05-23
;; Moved some functions to board_functions.rkt

#lang racket
(provide (all-defined-out))
(require "board_functions.rkt")

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

;; I/O: board% / boolean
;; Decides if sudoku has illegal (multiple) elements in a row, col and/or box.
(define (sudoku-solvable? board) 
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders)]
      [(valid-holder? (car rest-of-holders)) (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))

;; I/O: holder = list of element% / boolean
;; Decides if a holder contains any multiple elements or illegal elements (> 9 or < 0).
(define (valid-holder? holder) 
  (let ([holder-vals (map (lambda (e) (send e get-value))
                          holder)])
    (define (helper vals)
      (cond
        [(null? vals) #t]
        [(= 0 (car vals)) (helper (cdr vals))]
        [(or (> (car vals) 9) (< (car vals) 0) (member (car vals) (cdr vals))) #f]
        [else (helper (cdr vals))]))
    (helper holder-vals)))

;; I/O: board% / No output
;; Master function for calling solving procedures
(define (solve-sudoku! board)
  (cond
    [(sudoku-solved? board) (printf "Sudoku already solved!")] ;; Only run function if sudoku not yet solved.
    [(not (sudoku-solvable? board)) ;; Illegal sudoku.
     (printf "Error: duplicate elements in row, column and/or box.")] 
    [else
     (initialize-candidates! (send board get-elems)) ;; Makes candidate list and updates them by checking friends values.
     (solve-singletons! (send board get-elems))  ;; Try solving fully or partly by finding elems with single candidates.
     (unless (sudoku-solved? board)
       (solving-algorithm! board))
     (printf "Sudoku solved!~n ~n")
     (print-board board)]))
  

;; I/O: list of element% / No output
;; Finds all elements who has only one possible candidate and makes it his value. Also eliminates such a element from the friends list.
(define (solve-singletons! elems)
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
  (let ([first-e (car (send board get-elems))] [j 0])
    (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
      (set! j (+ j 1)) ;; iteration counter
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
  (unless (sudoku-solved? board)
    (solving-algorithm! board)
    (let* ([elems (send board get-elems)] [rnd-elem (list-ref elems (random 0 81))])
      (cond
        [(send rnd-elem user-val?)
         (clear-filled-elems! elems)
         (step-solve! board)]
        [else
         (send rnd-elem set-user-e! #t)
         (clear-filled-elems! elems)]))))

