#lang racket
(provide (all-defined-out))
(require "solver_functions.rkt")
(require "classes.rkt")
(require "sudoku_init.rkt")
(require "test_sudokus.rkt")

(define (solve-singletons! board)
  (let ([worked? #f])
    (for-each (lambda (e)
                (when (= 1 (length (send e get-candidates)))
                  (set! worked? #t)
                  (send e set-cand-to-val!)
                  (send e set-user-e! #t)))
              (send board get-elems))
    (initialize-candidates! board)
    (when worked? (solve-singletons! board))))

(define (get-holder-candidates holder)
  (flatten (map
            (lambda (e)
              (send e get-candidates))
            holder)))

(define (unique-candidate holder)
  (define (helper other-elems rest-of-elems)
    (cond
      [(null? rest-of-elems) (void)]
      [else
       (define (helper2 rest-of-cands)
         (cond
           [(null? rest-of-cands) (void)]
           [(member (car rest-of-cands) (get-holder-candidates other-elems))
            (helper2 (cdr rest-of-cands))]
           [else
            (send (car rest-of-elems) set-value! (car rest-of-cands))
            (send (car rest-of-elems) set-user-e! #t)]))
       (helper2 (send (car rest-of-elems) get-candidates))])
    (unless (null? rest-of-elems)
      (helper (remove (car rest-of-elems) holder) (cdr rest-of-elems))))
  (helper (cdr holder) holder))
  

(define (initialize-candidates! board)
  (define (helper rest-of-elems i)
    (if (> i 81)
        ;(printf "Sudoku-board set. ~n")
        (void)
        (begin
          (send (car rest-of-elems) update-candidates! #f)
          (helper (cdr rest-of-elems) (+ i 1)))))
  (helper (send board get-elems) 1))

(define (reset-board! board)
  (define (helper rest-of-elems i)
    (unless (> i 81)
      (send (car rest-of-elems) set-value! 0)
      (send (car rest-of-elems) reset-all-candidates!)
      (send (car rest-of-elems) set-user-e! #f)
      (helper (cdr rest-of-elems) (+ i 1))))
  (helper (send board get-elems) 1))

;; WILL BE REMOVED
(define (set-board! board sdk)
  (let ([elems (send board get-elems)])
    (reset-board! board)
    (for-each
     (lambda (i)
       (unless (= (list-ref sdk i) 0)
         (send (list-ref elems i) set-value! (list-ref sdk i))
         (send (list-ref elems i) set-user-e! #t)))
     (range 0 81))
    (initialize-candidates! board)))

(define (clear-filled-elems! board) ;; Clears all values that aren't flagged as user-values
  (define (helper elems)
    (cond
      [(null? elems) (void)]
      [(send (car elems) user-val?)
       (helper (cdr elems))]
      [else
       (send (car elems) set-value! 0)
       (send (car elems) reset-all-candidates!)
       (helper (cdr elems))]))
  (helper (send board get-elems)))

(define (step-solve! board)
  (unless (sudoku-solved? board)
    (solving-algorithm board)
    (let ([rnd-elem (list-ref (send board get-elems) (random 0 81))])
      (cond
        [(send rnd-elem user-val?)
         (clear-filled-elems! board)
         (step-solve! board)]
        [else
         (send rnd-elem set-user-e! #t)
         (clear-filled-elems! board)]))))
;(initialize-candidates! board)]))))

(define (solve-sudoku! board)
  (cond
    [(sudoku-solved? board) (printf "Sudoku already solved!")] ;; Only run function if sudoku not yet solved
    [(not (sudoku-solvable? board))
     (printf "Error: duplicate elements in row, column and/or box.")]
    [else
     (solve-singletons! board)
     (if (sudoku-solved? board)
         (printf "Sudoku solved by singleton check!")
         (solving-algorithm board))]))

;; WILl BE REMOVED: i
(define (solving-algorithm board)
  (let ([first-e (car (send board get-elems))] [i 1] [j 0] [startTime (current-inexact-milliseconds)])
    (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
      (set! j (+ j 1)) ;; iteration counter
      (cond
        [(equal? (send curr-e get-value) 'last)] ;; Made it to the last element
         
        [(send curr-e user-val?)
         (cond
           [(or (eqv? prev-e 'first) f)
            (send next-e update-candidates! #t)
            ;(when (< i 10) (printf "User-cell ~a. Going forward ~n" i)) (set! i (+ i 1)) ;; comment for debugging
            (helper curr-e next-e (send next-e get-nx-e) #t)]
           [else
            ;(when (< i 10) (printf "User-cell ~a. Going backward ~n" i)) (set! i (- i 1))
            (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
        [(send curr-e empty-cand?)
         ;(when (< i 10) (printf "Cell ~a. No candidates. Resetting candidates. Going backwards ~n" i)) (set! i (- i 1))
         (send curr-e set-value! 0)
         (send curr-e reset-candidates!)
         (helper (send prev-e get-pr-e) prev-e curr-e #f)]
        
        [else
         ;(when (> i 80) (printf "Cell ~a. Candidates: ~a. " i (send curr-e get-candidates)))
         (send curr-e set-cand-to-val!)
         (send next-e update-candidates! #t)
         ;(when (< i 10) (printf "Set value to ~a. Next element candidates ~a. Going forward. ~n" (send curr-e get-value) (send next-e get-candidates))) (set! i (+ i 1))
         (helper curr-e next-e (send next-e get-nx-e) #t)]))
    (helper 'first first-e (send first-e get-nx-e) #t)
    (printf "Number of iterations: ~a ~n Time: ~a ~n" j (- (current-inexact-milliseconds) startTime))))
       
(set-board! brd1 sdk1)
(set-board! brd2 sdk2)
(set-board! brd3 sdk3)
(set-board! brd4 sdk4)
(set-board! false-brd1 false-sdk1)
(set-board! false-brd2 false-sdk2)
