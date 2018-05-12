#lang racket
(provide (all-defined-out))
(require "classes.rkt")
(require "sudoku_init.rkt")

(define (valid-holder? holder) ;; decides if a holder contains any multiple elements or illegal elements (> 9 or < 0)
  (let ([holder-vals (map (lambda (e) (send e get-value))
                          holder)])
    (define (helper vals)
      (cond
        [(null? vals) #t]
        [(= 0 (car vals)) (helper (cdr vals))]
        [(or (> (car vals) 9) (< (car vals) 0) (member (car vals) (cdr vals))) #f]
        [else (helper (cdr vals))]))
    (helper holder-vals)))

(define (sudoku-solvable? board) ;; decides if sudoku has illegal (multiple) elements in a row, col and/or box
  (define (helper rest-of-holders)
    (cond
      [(null? rest-of-holders) #t]
      [(valid-holder? (car rest-of-holders)) (helper (cdr rest-of-holders))]
      [else #f]))
  (helper (send board get-holders)))
       
      
        

;; row1 = list of elements
;; rows = list of rows

(define (solved-holder? holder) ;; decides if a holder (row, column or box) contains the correct values (1-9 exactly once)
  (define (helper vals)
    (cond
      [(null? vals) #t]
      [(not (member (car vals) holder)) #f] ;; when it finds that one of the values 1-9 isn't in the holder, return #f
      [else (helper (cdr vals))]))
  (helper (range 1 10)))

;(define (multiple-solutions? board

(define (sudoku-solved? board) ;; returns #t if all holders are solved, and if there is a unique solution (UNIQUE CHECK DOESN'T WORK YET)
  (not (member #f (flatten
                   (map ;; this map gets rows, cols, boxes
                    (lambda (holders)
                      (map ;; this map gets row1, row2, row3..., col1, col2, col3..., box1, box2, box3...
                       (lambda (holder)
                         (solved-holder? (get-values holder))) ; checks if for example row1 contains correct elements
                       holders))
                    (send board get-holders))))))
           

(define i 1)
(define j 0)
(define iter 0)

(define (solving-algorithm board)
  (let ([first-e (car (send board get-elems))] [solved-once #f] [result (make-board)] [i 1] [j 0] [iter 0])
    (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
      (set! j (+ j 1)) ;; iteration counter
      (cond
        [(equal? (send curr-e get-value) 'last) ;; Made it to the last element
         (cond
           [solved-once
            (printf "Error: Multiple solutions found")]
           [else (set! solved-once #t)
                 (set! result (send board get-elems)) 
                 (set! iter j)
                 (set! i (- i 1))
                 (helper (send prev-e get-pr-e) prev-e curr-e #f)])]

        [(send curr-e user-val?)
         (cond
           [(and solved-once (equal? 'first prev-e))
            (printf "Sudoku solved! Number of iterations: ~a ~n" iter)
            (print-board result)]
           [(or (eqv? prev-e 'first) f)
            (send next-e update-candidates! #t)
            ;(when (< i 10) (printf "User-cell ~a. Going forward ~n" i)) (set! i (+ i 1)) ;; comment for debugging
            (helper curr-e next-e (send next-e get-nx-e) #t)]
           [else
            ;(when (< i 10) (printf "User-cell ~a. Going backward ~n" i)) (set! i (- i 1))
            (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
        [(send curr-e empty-cand?)
         (cond
           [(and solved-once (equal? 'first prev-e)) ;; Unique solution!
            (printf "Sudoku solved! Number of iterations: ~a ~n" iter)
            (print-board result)]
           [else
             ;(when (< i 10) (printf "Cell ~a. No candidates. Resetting candidates. Going backwards ~n" i)) (set! i (- i 1))
             (send curr-e set-value! 0)
             (send curr-e reset-candidates!)
             (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
        [else
         ;(when (< i 10) (printf "Cell ~a. Candidates: ~a. " i (send curr-e get-candidates)))
         (send curr-e set-cand-to-val!)
         (send next-e update-candidates! #t)
         ;(when (< i 10) (printf "Set value to ~a. Next element candidates ~a. Going forward. ~n" (send curr-e get-value) (send next-e get-candidates))) (set! i (+ i 1))
         (helper curr-e next-e (send next-e get-nx-e) #t)]))
    (helper 'first first-e (send first-e get-nx-e) #t)))



(define (print-board board) ;; prints sudoku board
  (for-each
   (lambda (i)
     (send (list-ref (send board get-elems) i) print-value)
     (when (= (remainder (+ i 1) 9) 0)
       (newline)))
   (range 0 81)))