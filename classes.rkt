#lang racket
(require racket/mpair)
(provide (all-defined-out))

(define element%
  (class object%
    (init-field
     [value 0]
     [candidates (list 1 2 3 4 5 6 7 8 9)]
     [init-candidates (list 1 2 3 4 5 6 7 8 9)]
     [prev-e 'first]
     [next-e 'last]
     [row '()]
     [col '()]
     [box '()]
     [user-val #f])

    (define/public (empty-cand?)
      (null? candidates))

    (define/public (get-value)
      value)

    (define/public (print-value)
      (printf "~a " value))

    (define/public (user-val?)
      user-val)

    (define/public (set-holder! holder type)
      (case type
        ['r (set! row holder)]
        ['c (set! col holder)]
        ['b (set! box holder)]))
    
    (define/public (get-row)
      row)

    (define/public (get-col)
      col)

    (define/public (get-box)
      box)

    (define/public (get-friends)
      (flatten (list row col box)))

    (define/public (get-friends-values)
      (map
       (lambda (e)
         (send e get-value))
       (flatten (list row col box))))

    (define/public (set-value! val)
      (set! value val))

    (define/public (set-user-e! b) ;; b is user value #t or #f
      (set! user-val b)
      (when b
        (set! candidates '())))
    

    (define/public (reset-candidates!)
      (set! candidates init-candidates))

    (define/public (reset-all-candidates!)
      (set! candidates (range 1 10))
      (set! init-candidates (range 1 10)))

    (define/public (update-candidates! type) ;;Two types: update candidates during solving (#t) or initial candidates (#f)
      (unless user-val
        (define (helper rest-of-candidates)
          (cond
            [(null? rest-of-candidates)
             (void)]
            [(member (car rest-of-candidates) (get-friends-values))
             (if type ;; #t -> update, #f -> initial candidates
                 (set! candidates (remove (car rest-of-candidates) candidates))
                 (begin
                   (set! init-candidates (remove (car rest-of-candidates) init-candidates))
                   (set! candidates init-candidates)))
             (helper (cdr rest-of-candidates))]
            [else
             (helper (cdr rest-of-candidates))]))
        (helper candidates)))
    
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
    (define/public (set-next-e! e)
      (set! next-e e))

    (super-new)))

(define board%
  (class object%
    (init-field
     [rows '()]
     [cols '()]
     [boxes '()]
     [elems '()])

    (define/public (get-rows)
      rows)
    (define/public (get-cols)
      cols)
    (define/public (get-boxes)
      boxes)
    (define/public (get-elems)
      elems)
    (define/public (get-elems-vals)
      (map (lambda (e) (send e get-value))
           elems))
    (define/public (get-holders)
      (append rows cols boxes))
    (super-new)))