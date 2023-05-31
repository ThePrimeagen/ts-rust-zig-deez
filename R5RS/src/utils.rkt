(define (append-i s i)
  (string-append s (number->string i)))

(define display-shadow display)

(define (display-l . args)
  (for-each (lambda (arg) (display-shadow arg)) args)
  (newline))


(define (format . args)
  (define out "")
  (for-each (lambda (arg)
              (cond
              ((number? arg) (set! out (string-append out (number->string arg))))
              ((string? arg) (set! out (string-append out arg)))
              (else (set! out (string-append out "void")))
              )) args)
  out)


(define (letter? char)
  (define ch (integer->char char))
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (char=? ch #\_)))

(define (digit? char)
  (define ch (integer->char char))
  (and (char>=? ch #\0) (char<=? ch #\9)))

