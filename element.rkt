#lang racket
(provide element%)

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

    (define/private (rm-duplicates lst)
      (cond
        [(null? lst) '()]
        [(member (car lst) (cdr lst))
         (rm-duplicates (cdr lst))]
        [else
         (cons (car lst) (rm-duplicates (cdr lst)))]))
      

    ;; CANNOT BE OPTIMIZED
    (define/public (get-friends-vals)
      (map (lambda (e)
             (send e get-value))
           (append row col box)))

    (define/public (set-value! val)
      (set! value val))

    (define/public (reset-candidates!)
      (set! candidates init-candidates))

    (define/public (reset-all-candidates!)
      (set! candidates (range 1 10))
      (set! init-candidates (range 1 10)))

    (define/public (set-user-e! b) ;; b is user value #t or #f
      (set! user-val b)
      (if b
          (set! candidates '())
          (reset-all-candidates!)))

    (define/public (update-candidates! type) ;;Two types: update candidates during solving (#t) or initial candidates (#f)
      (unless user-val
        (let ([friends-vals (get-friends-vals)])
          (set! candidates
                (filter (lambda (cand)
                          (not (member cand friends-vals)))
                        candidates))
          (unless type ;; #t -> update only, #f -> set initial candidates as well
            (set! init-candidates candidates)))))
              
                            
    
    (define/public (get-prev-e)
      prev-e)
    
    (define/public (get-next-e)
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