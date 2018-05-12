#lang racket
(require "classes.rkt")
(provide (all-defined-out))

(define (make-elements)
  (let
      ([elems (map (lambda (e) (new element%)) (range 0 82))])
    (send (first elems) set-next-e! (second elems))
    (send (list-ref elems 81) set-value! 'last)
    (for-each
     (lambda (i)
       (send (list-ref elems i) set-next-e! (list-ref elems (+ i 1)))
       (send (list-ref elems i) set-prev-e! (list-ref elems (- i 1))))
     (range 1 81))
    elems))

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

(define (print-elems holder)
  (for-each
   (lambda (e)
     (send e print-value))
   holder))

(define (get-values holder)
  (map
   (lambda (e)
     (send e get-value))
   holder))

(define (make-board)
  (let* ([elems (make-elements)]

         [box1 (make-box 0 elems)]
         [box2 (make-box 3 elems)]
         [box3 (make-box 6 elems)]
         [box4 (make-box 27 elems)]
         [box5 (make-box 30 elems)]
         [box6 (make-box 33 elems)]
         [box7 (make-box 54 elems)]
         [box8 (make-box 57 elems)]
         [box9 (make-box 60 elems)]
         [boxes (list box1 box2 box3 box4 box5 box6 box7 box8 box9)]

         [row1 (make-row 1 elems)]
         [row2 (make-row 2 elems)]
         [row3 (make-row 3 elems)]
         [row4 (make-row 4 elems)]
         [row5 (make-row 5 elems)]
         [row6 (make-row 6 elems)]
         [row7 (make-row 7 elems)]
         [row8 (make-row 8 elems)]
         [row9 (make-row 9 elems)]
         [rows (list row1 row2 row3 row4 row5 row6 row7 row8 row9)]

         [col1 (make-col 1 elems)]
         [col2 (make-col 2 elems)]
         [col3 (make-col 3 elems)]
         [col4 (make-col 4 elems)]
         [col5 (make-col 5 elems)]
         [col6 (make-col 6 elems)]
         [col7 (make-col 7 elems)]
         [col8 (make-col 8 elems)]
         [col9 (make-col 9 elems)]
         [cols (list col1 col2 col3 col4 col5 col6 col7 col8 col9)])

    (make-friends! rows 'r)
    (make-friends! cols 'c)
    (make-friends! boxes 'b)

    (new board%
         [rows rows]
         [cols cols]
         [boxes boxes]
         [elems elems])))
                 
(define brd1 (make-board))
(define brd2 (make-board))
(define brd3 (make-board))
