(define the-empty-stream '())
(define (head str) (car str))
(define (tail str) (force (cdr str)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream el str) (cons el (delay str)))))



(define (empty-stream? stream)
  (eq? the-empty-stream stream))

; Some utilities on streams
; -------------------------

;Expects finite stream !!!
(define (accumulate combiner nullvalue stream)
  (if (empty-stream? stream)
      nullvalue
      (combiner (head stream) (accumulate combiner nullvalue (tail stream)))))

; More general map-stream than in the slides:
;'op' is a procedure that takes (length streams) arguments.
; cfr. the map of R5RS on lists.
; Expects or infinite streams,
;         or finite streams of equal length !
(define (map-stream op . streams)
  (if (empty-stream? (head streams))
      the-empty-stream
      (cons-stream  (apply op (map head streams))
                    (apply map-stream (cons op (map tail streams))))))

; streamfilter
(define (streamfilter predicate stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((predicate (car stream))
         (cons-stream (head stream) (streamfilter predicate (tail stream))))
        (else (streamfilter predicate (tail stream)))))

; stream-for-each
; Expects a finite stream as second argument !!!
(define (stream-for-each proc stream)
  (if (empty-stream? stream)
      'done
      (begin (proc (head stream))
             (stream-for-each proc (tail stream)))))

; More general append-streams than in the slides:
; Expects a variable number of streams, of which all but the last
; should be finite !!!
(define (append-streams . streams)
  (if (null? streams)
      the-empty-stream
      (let ((first-stream (car streams))
            (rest-streams (cdr streams)))
        (if (empty-stream? first-stream)
            (apply append-streams rest-streams)
            (cons-stream (head first-stream)
                         (apply append-streams (cons (tail first-stream) rest-streams)))))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (+ low 1) high))))

; A B C  1 2 3  a b c
; ----> -----> ----->    (A 1 a) (B 2 b) (C 3 c)
;-------------------> => ------------------------>
(define (zip . streams)
  (cons-stream (map head streams)
               (apply zip (map tail streams))))

; print-stream : to print a finite stream
(define (print-stream s)
  (display "[")
  (stream-for-each (lambda (x) (display x) (display " ")) s)
  (display "] ")
  'ok)

; print-inf-stream : to print the n first elements of an infinite stream
; expects s to contain at least n elements ...
(define (print-inf-stream s n)
  (define (aux x s)
    (if (< x 1)
        (display "... ]\n")
        (begin (display (head s))(display " ")(aux (- x 1) (tail s)))))
  (display "[")
  (aux n s))

; Creates an infinite stream of user inputs
(define (make-read-stream)
  (letrec ((read-stream (cons-stream read read-stream)))
    (map-stream (lambda (f) (f))  read-stream)))


; cartesian-product : creates a (possibly infinite) stream
; of all possible combinations of all elements of all streams.
; (this is a more general version op 'pairs', as it allows more than 2
; stream as input (lists instead of cons) and because it allows also 
; infinite streams.
(define (cartesian-product . streams)
  
  (define (start-new-stream e)
    (map-stream (lambda (x) (cons e x)) (apply cartesian-product (cdr streams))))
  
  (define (aux stack started-streams c1)
    (cond
      ((null? started-streams)
       (cond
         ((and (empty-stream? c1) (null? stack)) the-empty-stream)
         ((empty-stream? c1) (aux '() (reverse stack) the-empty-stream))
         (else (aux '()
                    (reverse (cons (start-new-stream (head c1)) stack))
                    (tail c1)))))
      (else 
       (let ((first-stream (car started-streams))
             (rest-streams (cdr started-streams)))
         (let ((head-of-stream (head first-stream))
               (tail-of-stream (tail first-stream)))
           (if (empty-stream? tail-of-stream)
               (cons-stream head-of-stream (aux stack rest-streams c1))
               (cons-stream head-of-stream (aux (cons tail-of-stream stack) rest-streams c1))))))))
  
  (cond
    ((null? streams) (cons-stream the-empty-stream the-empty-stream))
    (else (aux '() '() (car streams) ))))

(define (pairs s1 s2)
  (map-stream (lambda (a-list) (cons (car a-list) (cadr a-list)))
              (cartesian-product s1 s2)))


(define (flatten s)
  (accumulate append-streams the-empty-stream s))


(define (flatmap f s)
  (flatten (map-stream f s)))

(define (list->stream lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst)
                   (list->stream (cdr lst)))))

;; Flatten for infinite stream (see slides for more information)
(define (flatten-inf stream)
  (define (accumulate-delayed com init stream)
    (if (empty-stream? stream)
        init
        (com 
         (head stream)
         (delay 
           (accumulate-delayed com init (tail stream))))))
  (define (interleave-delayed s1 s2)
    (if (empty-stream? s1)
        (force s2)
        (cons-stream 
         (head s1)
         (interleave-delayed 
          (force s2) 
          (delay (tail s1))))))
  (accumulate-delayed interleave-delayed
                      the-empty-stream
                      stream))

(define (stream-ref str pos)
  (if (<= pos 0) (head str) (stream-ref (tail str) (- pos 1))))