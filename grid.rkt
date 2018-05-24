;; This file has two purposes: creating the graphical interface, including text-fields and borders, and handling the user input.
;; It uses solver_functions.rkt to solve board and to detect illegal entries from user. This file is used to start the program.
;; Authors: Algirdas Bartkevicius & Emil Eriksson
;; Last update: 2018-05-23
;; Added comments

#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "sudoku_init.rkt")
(require "test_sudokus.rkt")
(require "board_functions.rkt")
(require racket/gui/base)

;; WINDOW APPEARANCE

(define h 270)
(define w 270)
(define *window* (new frame%
                      [width w]
                      [height (+ 30 h)]
                      [label "SudoSolver"]))

(define *font* (make-object font% 60 'default))

(define grid
  (flatten (map (lambda (hpanel)
                  (map (lambda (vpanel) 
                         (new vertical-panel%
                              [parent hpanel]
                              [style (list 'border)]))
                       (range 0 9)))
                (map
                 (lambda (i)
                   (new horizontal-panel%
                        [parent *window*]))
                 (range 0 9)))))

(define buttons-panel (new horizontal-panel%
                           [parent *window*]
                           [alignment (list 'center 'center)]))

;; BUTTON FUNCTIONS

(define control-event (new control-event%
                           [event-type '(text-field)]))

(define (solve-proc button event)
  (let ([time (current-inexact-milliseconds)])
    (cond
      [(sudoku-solved? user-board) (notify "Sudoku already solved!")] ;; Only run function if sudoku not yet solved
      [else
       (solve-sudoku! user-board)
       (set! time (number->string (round (- (current-inexact-milliseconds) time))))
       (set-text-fields! (send user-board get-elems-vals))
       (notify (string-append "Sudoku solved! Time: " time " ms."))])))

(define (step-proc button event)
  (if (sudoku-solved? user-board)
      (notify "Sudoku already solved!")
      (begin (step-solve! user-board)
             (set-text-fields! (send user-board get-elems-vals)))))

(define (reset-proc button event)
  (reset-board! user-board)
  (set-text-fields! (map (lambda (i) "")
                         (range 0 81))))

(define (generate-proc button event)
  (let ([sudoku (sample-sudoku)])
    (set-board! user-board sudoku)
    (set-text-fields! sudoku)))

;; Displays a popup box for the user
(define (notify message)
  (define dialog-box 
    (new dialog% 
         [label ""]
         [width 100] [height 100]
         [enabled #t]
         [style '(close-button)]
         [parent *window*]))
  (new text-field%
       [label #f]
       [parent dialog-box]
       [enabled #f]
       [init-value message])
  (new button%
       [parent dialog-box]
       [label "OK"]
       [callback 
        (lambda args
          (send dialog-box show #f))])
  (send dialog-box show #t))

;; BUTTONS

(define *solve-button* (new button%
                            [parent buttons-panel]
                            [label "Solve"]
                            [callback solve-proc]))

(define *step-solve-button* (new button%
                                 [parent buttons-panel]
                                 [label "Step Solve"]
                                 [callback step-proc]))

(define *reset-button* (new button%
                            [parent buttons-panel]
                            [label "Reset Sudoku"]
                            [callback reset-proc]))

(define *generate-button* (new button%
                               [parent buttons-panel]
                               [label "Generate Sudoku"]
                               [callback generate-proc]))

;; TEXT FIELD FUNCTIONS

(define (string-to-num value)
  (case value
    [("1") 1]
    [("2") 2]
    [("3") 3]
    [("4") 4]
    [("5") 5]
    [("6") 6]
    [("7") 7]
    [("8") 8]
    [("9") 9]
    [("") 0]
    [else 'error]))

;; I/O: No inputs / list of text-fields
;; Creates text-fields in the grid at appropriate places, and also specifies handle input (callback) in these text-fields.
(define (make-text-fields)
  (let
      ([text-fields
        (map (lambda (i)
               (new text-field%
                    [parent (list-ref grid i)]
                    [label #f]
                    [horiz-margin 0]
                    [vert-margin 0]
                    [font *font*]
                    [callback
                     (lambda (this control-event)
                       (let ([val (string-to-num (send this get-value))])
                         (cond
                           [(and
                             (member val (send (send user-board get-elem i) get-friends-vals)) ;; checks if user tries to enter a duplicate value
                             (not (eqv? val 0))) ;; 0 doesn't count as a duplicate
                            (notify "Error: value already in row/col/box")
                            (send this set-value "")
                            (send user-board set-value! i 0)]
                           [(eqv? val 'error) ;; checks if user tries to enter an invalid value
                            (notify "Error: invalid character. Please enter a value from 1-9.")
                            (send this set-value "")
                            (send user-board set-value! i 0)]
                           [else
                            (send user-board set-value! i val)])))]))
             (range 0 81))])
    text-fields))

;; I/O: sudoku values from list / sudoku values in grid
;; Takes in a list of values and sets text fields to these values
(define (set-text-fields! vals)
  (for
      ([i text-fields] [j vals])
    (if (and (number? j) (not (= 0 j)))
        (send i set-value (number->string j))
        (send i set-value ""))))

(define user-board (make-board))
(define text-fields (make-text-fields))