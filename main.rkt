#lang racket
(require "classes.rkt")
(require "world_init_test.rkt")
(require "solver_functions.rkt")

(define sdk1 (list 5 3 0 0 7 0 0 0 0
                   6 0 0 1 9 5 0 0 0
                   0 9 8 0 0 0 0 6 0
                   8 0 0 0 6 0 0 0 3
                   4 0 0 8 0 3 0 0 1
                   7 0 0 0 2 0 0 0 6
                   0 6 0 0 0 0 2 8 0
                   0 0 0 4 1 9 0 0 5
                   0 0 0 0 8 0 0 7 9))

(define sdk2 (list 5 1 2 7 0 0 0 0 0
                   0 7 0 0 4 0 1 0 2
                   8 9 0 0 0 0 0 5 0
                   0 3 0 4 0 2 0 1 0
                   0 0 8 9 6 1 3 0 0
                   0 5 0 8 0 7 0 4 0
                   0 8 0 0 0 0 0 3 9
                   4 0 3 0 9 0 0 2 0
                   0 0 0 0 0 4 8 6 1))

(define (initialize-candidates! board)
  (define (helper rest-of-elems i)
    (if (> i 81)
        (printf "Candidates assigned. ")
        (begin
          (send (car rest-of-elems) update-candidates!)
          (helper (cdr rest-of-elems) (+ i 1)))))
  (helper board 1))

(define (reset-board! board)
  (define (helper rest-of-elems i)
    (unless (> i 81)
      (send (car rest-of-elems) set-value! 0)
      (send (car rest-of-elems) reset-candidates!)
      (send (car rest-of-elems) set-user-e! #f)
      (helper (cdr rest-of-elems) (+ i 1))))
  (helper board 1))

(define (set-board! board elems)
  (reset-board! board)
  (for-each
   (lambda (i)
     (unless (= (list-ref elems i) 0)
       (send (list-ref board i) set-value! (list-ref elems i))
       (send (list-ref board i) set-user-e! #t)))
   (range 0 81))
  (initialize-candidates! board))

(define (print-board board)
  (for-each
   (lambda (i)
     (send (list-ref board i) print-value)
     (when (= (remainder (+ i 1) 9) 0)
       (newline)))
   (range 0 81)))
         
         

(set-board! brd sdk2)
