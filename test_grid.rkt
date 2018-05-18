#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "sudoku_init.rkt")
(require "main.rkt")
(require "test_sudokus.rkt")
(require racket/gui/base)
(require racket/draw)

;(set-board! brd sdk3)

(define h 270)
(define w 270)

(define *window* (new frame%
                      [width w]
                      [height (+ 30 h)]
                      [label "SudoSolver"]))

(send *window* show #t)

(define (notify message)
  (define dialog-box 
    (new dialog% 
         [label ""]
         [width 200]
         [height 100]
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

(define control-event (new control-event%
                           [event-type '(text-field)]))

(define *font* (make-object font% 60 'default))

;(define *color* (make-object color% 255 0 0 1.0))

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
    
(define (make-text-fields board)
  (let
      ([text-fields
        (map
         (lambda (i)
           (new text-field%
                [parent (list-ref grid i)]
                [label #f]
                [horiz-margin 0]
                [vert-margin 0]
                [font *font*]
                [callback
                 (lambda (this control-event)
                   (send brd set-value! i (correct-value (send this get-value))))]))
         (range 0 81))])
    text-fields))

(define text-fields
  (make-text-fields brd))

(define (set-text-field-val i val)
  (send (list-ref text-fields i) set-value val))

(define (get-text-field-vals text-fields)
  (map
   (lambda (txt)
     (send txt get-value))
   text-fields))

(define (set-text-fields! text-fields elems)
  (for
      ([i text-fields] [j elems])
    (if (and (number? j) (not (= 0 j)))
        (send i set-value (number->string j))
        (send i set-value ""))))

(define buttons-panel (new horizontal-panel%
                           [parent *window*]
                           [alignment (list 'center 'center)]))

(define (solve-proc button event)
  (solve-sudoku! brd)
  (set-text-fields! text-fields (send brd get-elems-vals))
  (notify "Sudoku solved!"))
  ;(notify "Sudoku solved! Number of iterations: ~a" j")


(define (step-proc button event)
  (step-solve! brd)
  (set-text-fields! text-fields (send brd get-elems-vals)))

(define (reset-proc button event)
  (reset-board! brd)
  (set-text-fields! text-fields (map
                                 (lambda (i)
                                   "")
                                 (range 0 81))))

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

(set-text-fields! text-fields (send brd get-elems-vals))

