#lang racket

(define (solving-algorithm board)
  (let ([first-e (car (send board get-elems))] [solved-once? #f] [result (make-board)] [i 1] [j 0] [iter 0])
    (define (helper prev-e curr-e next-e f) ;; f indicates if the algorithm is going backward or forward. f = #t: forward, f = #f: backward
      (set! j (+ j 1)) ;; iteration counter
      (cond
        [(equal? (send curr-e get-value) 'last) ;; Made it to the last element
         (cond
           [solved-once?
            (printf "Error: Multiple solutions found")]
           [else (set! solved-once? #t)
                 (set-board! result (send board get-elems-vals)) 
                 (set! iter j)
                 ;(set! i (- i 1))
                 (helper (send prev-e get-pr-e) prev-e curr-e #f)])]

        [(send curr-e user-val?)
         (cond
           [(and solved-once? (equal? 'first prev-e))
            (printf "Sudoku solved! Number of iterations: ~a ~n" j)
            (set-board! board (send result get-elems-vals))
            (print-board board)]
           [(or (eqv? prev-e 'first) f)
            (send next-e update-candidates! #t)
            ;(when (< i 10) (printf "User-cell ~a. Going forward ~n" i)) (set! i (+ i 1)) ;; comment for debugging
            (helper curr-e next-e (send next-e get-nx-e) #t)]
           [else
            ;(when (< i 10) (printf "User-cell ~a. Going backward ~n" i)) (set! i (- i 1))
            (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
        [(send curr-e empty-cand?)
         (cond
           [(and solved-once? (equal? prev-e 'first)) ;; Unique solution!
            (printf "Sudoku solved! Number of iterations: ~a ~n" j)
            (set-board! board (send result get-elems-vals))
            (print-board board)]
           [else
            ;(when (< i 10) (printf "Cell ~a. No candidates. Resetting candidates. Going backwards ~n" i)) (set! i (- i 1))
            (send curr-e set-value! 0)
            (send curr-e reset-candidates!)
            (helper (send prev-e get-pr-e) prev-e curr-e #f)])]
        
        [else
         (when (> i 80) (printf "Cell ~a. Candidates: ~a. " i (send curr-e get-candidates)))
         (send curr-e set-cand-to-val!)
         (send next-e update-candidates! #t)
         ;(when (< i 10) (printf "Set value to ~a. Next element candidates ~a. Going forward. ~n" (send curr-e get-value) (send next-e get-candidates))) (set! i (+ i 1))
         (helper curr-e next-e (send next-e get-nx-e) #t)]))
    (helper 'first first-e (send first-e get-nx-e) #t)))