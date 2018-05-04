#lang racket
(require racket/mpair)
(require htdp/matrix)

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

(define mtx (make-matrix 9 9 (list 5 3 0 0 7 0 0 0 0
                                   6 0 0 1 9 5 0 0 0
                                   0 9 8 0 0 0 0 6 0
                                   8 0 0 0 6 0 0 0 3
                                   4 0 0 8 0 3 0 0 1
                                   7 0 0 0 2 0 0 0 6
                                   0 6 0 0 0 0 2 8 0
                                   0 0 0 4 1 9 0 0 5
                                   0 0 0 0 8 0 0 7 9)))

(define (print-row mtx row#)
  (define (helper col)
    (unless (> col 8)
      (display (matrix-ref mtx (- row# 1) col))
      (helper (+ col 1))))
  (helper 0))

(define (print-col col#)
  (define (helper row)
    (unless (> row 8)
      (println (matrix-ref mtx (- row# 1) col))
      (helper (+ col 1))))
  (helper 0))

