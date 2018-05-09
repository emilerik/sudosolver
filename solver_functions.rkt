#lang racket
(provide (all-defined-out))
(require "classes.rkt")
(require "sudoku_init.rkt")


(define (correct-holder? holder) ;; decides if a holder (row, column or box) contains the correct values (1-9 exactly once)
  (define (helper vals)
    (cond
      [(null? vals) #t]
      [(not (member (car vals) holder)) #f] ;; when it finds that one of the values 1-9 isn't in the holder, return #f
      [else (helper (cdr vals))]))
  (helper (range 1 10)))

;; row1 = list of elements
;; rows = list of rows

(define (sudoku-solved?) ;; this function has three components: a "holder" inside "holders", for example row1 inside rows
  (not (member #f (flatten
                   (map ;; this map gets rows, cols, boxes
                    (lambda (holders)
                      (map ;; this map gets row1, row2, row3..., col1, col2, col3..., box1, box2, box3...
                       (lambda (holder)
                         (correct-holder? (get-values holder))) ; checks if for example row1 contains correct elements
                       holders))
                    (list rows cols boxes))))))

(define i 1)
(define j 0)


(define (solve-sudoku board)
  (if (sudoku-solved?)
      (printf "Sudoku already solved!") ;; Only run function if sudoku not yet solved!
      (let ((first-ele (car board)))
        (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
          (set! j (+ j 1)) ;; iteration counter
          (cond
            [(equal? (send curr-e get-nx-e) "") ;; Made it to the last element
             (printf "Sudoku solved! Number of iterations: ~a ~n ~n" j)
             (print-board)]

            [(send curr-e user-val?)
             (cond
               [(or (eqv? prev-e "") f) 
                (send next-e update-candidates! #t)
                ;(printf "User-cell ~a. Going forward ~n" i) ;; comment for debugging
                (helper curr-e next-e (send next-e get-nx-e) #t)]
               [else
                ;(printf "User-cell ~a. Going backward ~n" i)
                (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
            [(send curr-e empty-cand?)
             ;(printf "Cell ~a. No candidates. Resetting candidates. Going backwards ~n" i)
             (send curr-e set-value! 0)
             (send curr-e reset-candidates!)
             (helper (send prev-e get-pr-e) prev-e curr-e #f)]
        
            [else
             ;(printf "Cell ~a. Candidates: ~a. " i (send curr-e get-candidates))
             (send curr-e set-cand-to-val!)
             (send next-e update-candidates! #t)
             ;(printf "Set value to ~a. Next element candidates ~a. Going forward. ~n" (send curr-e get-value) (send next-e get-candidates))
             (helper curr-e next-e (send next-e get-nx-e) #t)]))
        (helper '() first-ele (send first-ele get-nx-e) #t))))

(define (print-board) ;; prints sudoku board
  (for-each
   (lambda (i)
     (send (list-ref brd i) print-value)
     (when (= (remainder (+ i 1) 9) 0)
       (newline)))
   (range 0 81)))