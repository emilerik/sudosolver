#lang racket
(require "classes.rkt")
(provide (all-defined-out))

(define (make-board)
  (let
      ([elems (map (lambda (e) (new element%)) (range 0 81))])
    (send (first elems) set-next-e! (second elems))
    (send (list-ref elems 80) set-prev-e! (list-ref elems 79)) 
    (for-each
     (lambda (i)
       (send (list-ref elems i) set-next-e! (list-ref elems (+ i 1)))
       (send (list-ref elems i) set-prev-e! (list-ref elems (- i 1))))
     (range 1 80))
    elems))

(define elems (make-board))

(define (make-row n elems)
  (define (helper i)
    (if (> i 8)
        '()
        (cons (list-ref elems (+ (* (- n 1) 9) i))
              (helper (+ i 1)))))
  (helper 0))

(define (make-col n elems)
  (define (helper i)
    (if (> i 8)
        '()
        (cons (list-ref elems (+ (- n 1) (* i 9)))
              (helper (+ i 1)))))
  (helper 0))

(define (make-box s elems)
  (for/list
      ([i (list s       (+ s 1) (+ s 2)
                (+ s 9) (+ s 10) (+ s 11)
                (+ s 18) (+ s 19) (+ s 20))])
    (list-ref elems i)))

(define (make-friends! holders type)
  (for-each
   (lambda (holder)
     (define (helper rem-elems)
       (unless (null? rem-elems)
         (send (car rem-elems) set-holder! (remove (car rem-elems) holder) type)
         (helper (cdr rem-elems))))
     (helper holder))
   holders))
                  
(define (set-tmp-board! board)
  (for-each
   (lambda (i)
     (send (list-ref board i) set-value! (+ i 1)))
   (range 0 81)))

(define (print-elems lst)
  (for-each
   (lambda (e)
     (send e print-value))
   lst))

(define brd (make-board))

(define box1 (make-box 0 brd))
(define box2 (make-box 3 brd))
(define box3 (make-box 6 brd))
(define box4 (make-box 27 brd))
(define box5 (make-box 30 brd))
(define box6 (make-box 33 brd))
(define box7 (make-box 54 brd))
(define box8 (make-box 57 brd))
(define box9 (make-box 60 brd))
(define boxes (list box1 box2 box3 box4 box5 box6 box7 box8 box9))

(define row1 (make-row 1 brd))
(define row2 (make-row 2 brd))
(define row3 (make-row 3 brd))
(define row4 (make-row 4 brd))
(define row5 (make-row 5 brd))
(define row6 (make-row 6 brd))
(define row7 (make-row 7 brd))
(define row8 (make-row 8 brd))
(define row9 (make-row 9 brd))
(define rows (list row1 row2 row3 row4 row5 row6 row7 row8 row9))

(define col1 (make-col 1 brd))
(define col2 (make-col 2 brd))
(define col3 (make-col 3 brd))
(define col4 (make-col 4 brd))
(define col5 (make-col 5 brd))
(define col6 (make-col 6 brd))
(define col7 (make-col 7 brd))
(define col8 (make-col 8 brd))
(define col9 (make-col 9 brd))
(define cols (list col1 col2 col3 col4 col5 col6 col7 col8 col9))

(make-friends! rows 'r)
(make-friends! cols 'c)
(make-friends! boxes 'b)


