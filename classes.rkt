#lang racket
(require racket/mpair)
(provide (all-defined-out))

(define element%
  (class object%
    (init-field
     [value 0]
     [candidates (list 1 2 3 4 5 6 7 8 9)]
     [prev-e ""]
     [next-e ""]
     [user-val #f])

    (define/public (empty-cand?)
      (null? candidates))

    (define/public (get-value)
      value)

    (define/public (user-val?)
      user-val)

    (define/public (set-value! val)
      (set! value val))

    (define/public (set-user-e!)
      (set! user-val #t))

    (define/public (reset-candidates!)
      (set! candidates (list 1 2 3 4 5 6 7 8 9)))

    (define/public (get-pr-e)
      prev-e)
    
    (define/public (get-nx-e)
      next-e)

    (define/public (get-candidates)
      candidates)

    (define/public (rm-candidate! val)
      (set! candidates
            (remove val candidates)))

    (define/public (set-cand-to-val!)
      (set! value (car candidates))
      (set! candidates (cdr candidates)))

    (define/public (set-prev-e! e)
      (set! prev-e e))

  (super-new)))

(define row%
  (class object%
    (init-field
     [row-number 0]
     [elements '()])
          
    (define/public (get-element pos)
       (list-ref elements (- pos 1)))

    (define/public (delete-element-candidate! pos val)
      (send (get-element pos) rm-candidate! val))
            
    (define/public (set-element! pos val)
      (send (get-element pos) set-value! val))

    (define/public (get-row)
      elements)

    (define/public (get-row-vals)
      (map
       (lambda (ele)
         (send ele get-value))
       elements))
    
    (define/public (row-contains? number)
      (if (member number
                  (get-row))
          #t
          #f))
    
    (super-new)))

(define column%
  (class object%
    (init-field
     [col-num 0]
     [rows (list)])

    (define/public (get-element pos)
      (send (list-ref rows (- pos 1)) get-element col-num))

    (define/public (get-col)
      (define (helper rem-rows)
        (if (null? rem-rows)
            '()
            (cons (send (first rem-rows) get-element col-num)
                  (helper (rest rem-rows)))))
      (helper rows))

    (define/public (col-contains? number)
      (if (member number
                  (get-col))
          #t
          #f))

    (super-new)))

(define box%
  (class object%
    (init-field
     [box-num 0]
     [rows (list)]
     [cols (list)])

    (define/public (get-box)
      (define (helper rem-rows rem-cols)
        (cond
          [(null? rem-rows)
           '()]
          [(null? rem-cols)
           (helper (cdr rem-rows) cols)]
          [else
           (cons (send (first rem-rows) get-element (first rem-cols))
                 (helper rem-rows (rest rem-cols)))]))
      (helper rows cols))

    (define/public (box-contains? number)
      (if (member number
                  (get-box))
          #t
          #f))

    (super-new)))

(define board%
  (class object%
    (init-field
     [rows (list)])

    (define/public (get-element row col)
      (send (list-ref rows (- row 1)) get-element col))

    (define/public (set-element! row col val)
      (send (get-element row col) set-value! val))

    (define/public (get-board-vals)
      (for-each
       (lambda (row)
         (println (send row get-row-vals)))
       rows))

    (define/public (get-board-elements)
      (flatten
       (map
       (lambda (row)
         (send row get-row))
       rows)))

    (define/public (set-board! lst)
      (define (helper row col values-lst)
        (cond
          [(or (> row 9) (null? values-lst))
           (void)]
          [(> col 9)
           (helper (+ row 1) 1 values-lst)]
          [(= 0 (first values-lst))
           (helper row (+ col 1) (rest values-lst))]
          [else
           (send (get-element row col) set-value! (first values-lst))
           (send (get-element row col) set-user-e!)
           (helper row (+ col 1) (rest values-lst))]))
      (helper 1 1 lst))


    (super-new)))

    
     
