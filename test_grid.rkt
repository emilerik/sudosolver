#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "sudoku_init.rkt")
(require "main.rkt")
(require racket/gui/base)

(define h 270)
(define w 270)

(define *window* (new frame%
                      [width w]
                      [height (+ 30 h)]
                      [label "SudoSolver"]))

(send *window* show #t)

(define grid
  (flatten (map
            (lambda (hpanel)
              (map
               (lambda (vpanel) 
                 (new vertical-panel%
                      [parent hpanel]
                      [style (list 'border)]))
               (range 0 9)))
            (map
             (lambda (i)
               (new horizontal-panel%
                    [parent *window*]))
             (range 0 9)))))

(define control-event (new control-event%
                           [event-type '(text-field)]))

(define *font* (make-object font% 60 'default))

(define (correct-value value)
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
    [else 0]))
    
(define (grid-text-fields board)
  (map
   (lambda (i)
     (new text-field%
          [parent (list-ref grid i)]
          [init-value (number->string (send board get-value i))]
          [label #f]
          [horiz-margin 0]
          [vert-margin 0]
          [font *font*]
          [callback
           (lambda (this control-event)
             (send brd set-value! i (correct-value (send this get-value))))]))
             ;(send (list-ref brd i) set-value! (correct-value (send this get-value))))]))
   (range 0 81)))

(grid-text-fields brd)

(define (set-text-field-val i val)
  (send (list-ref (grid-text-fields) i) set-value val))
(define (get-text-field-vals text-fields)
  (map
   (lambda (txt)
     (send txt get-value))
   text-fields))

(define Buttons-panel (new horizontal-panel%
                           [parent *window*]
                           [alignment (list 'center 'center)]))

(define (Solve-proc button event)
  (solve-sudoku! brd))

(define (Step-proc button event)
  (send button set-label "Klick fungerade!"))

(define (Generate-proc button event)
  (send button set-label "Klick fungerade!"))

(define *Solve-button* (new button%
                            [parent Buttons-panel]
                            [label "Solve"]
                            [callback Solve-proc]))

(define *Step-Solve-Button* (new button%
                                 [parent Buttons-panel]
                                 [label "Step Solve"]
                                 [callback Step-proc]))

(define *Generate-Sudoku-Button* (new button%
                                      [parent Buttons-panel]
                                      [label "Generate Sudoku"]
                                      [callback Generate-proc]))
