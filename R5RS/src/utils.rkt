(#%require (only racket/base error make-hash hash-set! hash-update! hash-ref hash-count hash-for-each))


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

(define (string-replace str old new)
  (define len (string-length str))
  (define old-len (string-length old))
  (define result '())
  (define (inner i result)
    (cond
        ((>= i len)
         (list->string (reverse result)))
        ((and (<= (+ i old-len) len)
              (string=? (substring str i (+ i old-len)) old))
         (inner (+ i old-len) (append (string->list new) result)))
        (else
         (inner (+ i 1) (cons (string-ref str i) result)))))
  (inner 0 result))


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

(define (pad-zero-left lst l)
  (if (< (length lst) l)
      (cons 0 (pad-zero-left lst (- l 1)))
      lst))

(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

(define (remove-first-n lst n)
  (cond ((> n (length lst)) lst)
        ((= n 0) lst)
        (else (remove-first-n (cdr lst) (- n 1)))))

(define (last-n lst n)
  (if(> n (length lst)) lst (remove-first-n lst (- (length lst) n))))


; INT FUNCTIONS
(define (power base exponent)
  (if (= exponent 0)
      1
      (* base (power base (- exponent 1)))))


; BIT FUNCTIONS
(define (number-list->bit-list lst)
  (flatten (map number->bit-list lst)))

(define (number->bit-list number)
  (let* ((binary-string (number->string number 2)))
    (pad-zero-left (map (lambda (bit-char) (string->number (string bit-char)))
         (string->list binary-string)) 8)))

(define (string->bit-list s)
  (map number-to-bit-list (map char->integer (string->list s))))

(define (xor b1 b2)
  (cond ((and (= b1 1) (= b2 0)) 1)
        ((and (= b1 0) (= b2 1)) 1)
        (else 0)))

(define (bitwise-and b1 b2)
  (if (and (= b1 1) (= b2 1)) 1 0))

(define (fnv-xor hash bit)
  (define (inner hash bit index out)
    (cond
      ((= index -1) out)
      ((< index 8) (inner (cdr hash) (cdr bit) (- index 1) (add-to-list out (xor (car hash) (car bit)))))
      (else (inner (cdr hash) bit (- index 1) (add-to-list out (car hash))))))
  (inner hash bit 63 '()))
    
(define (bit-list->number lst)
  (define (inner lst number)
    (if (null? lst) number (inner (cdr lst) (+ number (* (car lst) (power 2 (- (length lst) 1)))))))
  (inner lst 0))

; CRIES IN ZERO PERFORMANCE
; I don't want to implement binairy multplication right now
(define (fnv-mul x y)
  (last-n (pad-zero-left (number->bit-list (* (bit-list->number x) (bit-list->number y))) 64) 64))


; HASH FUNCTION
(define FNV-PRIME (pad-zero-left (number->bit-list 1099511628211) 64))
(define FNV-BASIS (pad-zero-left (number->bit-list 14695981039346656037) 64))

; PERFORMANCE TO HELL BABY
(define (fnv-hash s)
  (let* ((bytes (map char->integer (string->list s)))
         (hash FNV-BASIS)) (begin
  (for-each (lambda (byte)
              (set! hash (fnv-mul hash FNV-PRIME))
              (set! hash (fnv-xor hash (number->bit-list byte)))) bytes) (bit-list->number hash))))