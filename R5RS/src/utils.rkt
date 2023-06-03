(#%require (only racket/base time error make-hash hash-set! hash-update! hash-ref))

(define (loader name)
  (load (string-append (string-append "../src/" name) ".rkt")))

; TAGGED LIST
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))


; STRING FUNCTIONS
(define (append-i s i)
  (string-append s (number->string i)))

(define (display-chain . args) (for-each (lambda (arg) (display arg)) args))

(define (display-nl t) (display t) (newline))

(define (display-l . args)
  (for-each (lambda (arg) (display-chain arg " ")) args)
  (newline)
  (newline))

(define (format . args)
  (define out "")
  (for-each (lambda (arg)
              (cond
              ((number? arg) (set! out (string-append out (number->string arg))))
              ((string? arg) (set! out (string-append out arg)))
              (else (begin (display-l "Unknown found in format, displaying for completion" arg)(set! out (string-append out "unknown"))))
              )) args)
  out)

(define (string-join del lst)
  (define out "")
  (define index 0)
  (for-each (lambda (el) (set! index (+ 1 index)) (if (= index (length lst)) (set! out (string-append out el)) (set! out (string-append (string-append out el) del)))) lst)
  out)


; CHECKERS
(define (letter? char)
  (define ch (integer->char char))
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (char=? ch #\_)))

(define (digit? char)
  (define ch (integer->char char))
  (and (char>=? ch #\0) (char<=? ch #\9)))

(define (bool? b)
  (or (eq? b #f) (eq? b #t)))


; LIST FUNCTIONS
(define (get-nth-element lst n)
  (if (or (null? lst) (< n 0))
      (error "Invalid index or empty list")
      (if (= n 0)
          (car lst)
          (get-nth-element (cdr lst) (- n 1)))))

(define (add-to-list lst element)
  (if (null? lst)
      (list element)
      (append lst (list element))))