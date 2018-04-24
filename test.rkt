#lang racket
(require racket/mpair)

(define (remove-cand mlst cand)
  (cond
    ((null? mlst)
     (void))
    ((eqv? (mcar mlst) cand)
     (set-mcar! mlst (mcdr mlst)))
    (else
     (remove-cand (mcdr mlst) cand))))

(define (delete-element-candidate! pos candidate)
  (hash-set! *elements* pos (remove
                            candidate
                            (hash-ref *elements* pos))))

(define *elements* (make-hash))

(hash-set! *elements* 1 (list 1 2 3 4 5 6 7 8 9))

(define row%
  (class object%
    (init-field
     [row-number 0])

    (define *elements* (make-hash))

    (hash-set! *elements* 1 (list 1 2 3 4 5 6 7 8 9))
    (hash-set! *elements* 2 8)
    (hash-set! *elements* 3 (list 1 2 3 4 5 6 7 8 9))
          
    (define/public (get-element-candidates pos)
      (hash-ref *elements* pos))

    (define/public (delete-element-candidate! pos candidate)
      (hash-set! *elements* pos (remove
                                candidate
                                (hash-ref *elements* pos))))

    (define/public (set-element! pos value)
      (hash-set! *elements* pos value))

    (define/public (hsh-values)
      (hash-values *elements*))
    
    (define/public (row-contains? number)
      (if (member number
              (hash-values *elements*))
          #t
          #f))
    
    (super-new)))

(define row1
  (new row%
       [row-number 1]))

(define row2
  (new row%
       [row-number 2]))