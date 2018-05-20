#lang racket
(provide board%)

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
      (let ([elem (list-ref elems i)])
        (send elem set-value! val)
        (if (= val 0)
            (send elem set-user-e! #f) 
            (send elem set-user-e! #t))))
    (super-new)))