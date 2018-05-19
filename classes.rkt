#lang racket
(require racket/mpair)
(provide (all-defined-out))

(define element%
  (class object%
    (init-field
     [value 0]
     [guessed-candidates '()]
     [init-candidates (range 0 10)]
     [removed-candidates '()]
     [prev-e 'first]
     [next-e 'last]
     [row '()]
     [col '()]
     [box '()]
     [user-val #f])

    (define/public (empty-cand?)
      (null? (get-candidates)))

    (define/public (get-value)
      value)

    ;;ISN'T USED
    (define/public (print-value)
      (printf "~a " value))

    (define/public (user-val?)
      user-val)

    (define/public (set-holder! holder type)
      (case type
        ['r (set! row holder)]
        ['c (set! col holder)]
        ['b (set! box holder)]))
    
    ;(define/public (get-row)
    ;  row)

    ;(define/public (get-col)
    ;  col)

    ;(define/public (get-box)
    ;  box)

    (define/public (get-friends)
      (flatten (list row col box)))

    ;; CAN BE OPTIMIZED
    (define/public (get-friends-vals)
      (map
       (lambda (e)
         (send e get-value))
       (flatten (list row col box))))

    (define/public (set-value! val)
      (set! value val))

    (define/public (set-user-e! b) ;; b is user value #t or #f
      (set! user-val b)
      (when b
        (set! init-candidates '())))
    

    (define/public (reset-candidates!)
      (set! guessed-candidates '()))

    (define/public (reset-all-candidates!)
      (set! init-candidates (range 1 10)))

    (define/public (update-candidates! type) ;;Two types: update candidates during solving (#t) or initial candidates (#f)
      (unless user-val
        (define (helper rest-of-candidates)
          (cond
            [(null? rest-of-candidates)
             (void)]
            [(member (car rest-of-candidates) (get-friends-vals))
             (if type ;; #t -> update, #f -> initial candidates
                 (void)
                 ;(set! candidates (remove (car rest-of-candidates) candidates))
                 (set! init-candidates (remove (car rest-of-candidates) init-candidates)))
             (helper (cdr rest-of-candidates))]
            [else
             (helper (cdr rest-of-candidates))]))
        (helper (get-candidates))))
    
    (define/public (get-pr-e)
      prev-e)
    
    (define/public (get-nx-e)
      next-e)

    (define/public (get-candidates)
      (filter (lambda (cand)
                (not (member cand (append removed-candidates guessed-candidates))))
              init-candidates))

    ;;TEMPORARY
    (define/public (get-rm-candidates)
      removed-candidates)
    (define/public (get-init-candidates)
      init-candidates)

    (define/public (rm-candidate! val)
      (when (= value 0)
        (set! removed-candidates (cons val removed-candidates))))
    (define/public (add-candidate! val)
      (when (= value 0)
        (set! removed-candidates (remove val removed-candidates))))
    
    (define/public (set-cand-to-val!)
      (let ([cand (car (get-candidates))])
        (set! value cand)
        (set! guessed-candidates (cons cand guessed-candidates))))

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

    (define/public (get-value i)
      (send (list-ref elems i) get-value))

    (define/public (set-value! i val)
      (send (list-ref elems i) set-value! val)
      (if (= val 0)
          (send (list-ref elems i) set-user-e! #f) 
          (send (list-ref elems i) set-user-e! #t)))
    (super-new)))