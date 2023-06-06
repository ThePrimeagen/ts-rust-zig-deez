(load "../src/utils.rkt")

; LEXER
; (INPUT pos read-pos char)
(define (new-lexer input)
  (define l (list 'lexer input 0 0 0))
  (lexer-read-char l)
  l
  )

(define (lexer? l)
  (tagged-list? l 'lexer))


; GETTERS
(define (lexer-input l)
  (cadr l))

(define (lexer-pos l)
  (caddr l))

(define (lexer-read-pos l)
   (cadddr l))

(define (lexer-char l)
  (cadr (cdddr l)))

(define (lexer-byte l)
  (char->integer (string-ref (lexer-input l) (lexer-read-pos l))))


; SETTERS
(define (set-input! l input)
  (set-car! (cdr l) input))

(define (set-pos! l pos)
  (set-car! (cddr l) pos))

(define (set-read-pos! l read-pos)
  (set-car! (cdddr l) read-pos))

(define (set-char! l char)
  (set-car! (cdr (cdddr l)) char))


; METHODS
(define (inc-read-pos! l)
  (set-read-pos! l (+ (lexer-read-pos l) 1)))

(define (lexer-peak-char l)
  (if (>= (lexer-read-pos l) (string-length (lexer-input l)))
      0
      (lexer-byte l)))

(define (lexer-peak-back-char l)
  (if (= (lexer-pos l) 0)
      0
      (char->integer (string-ref (lexer-input l) (- (lexer-pos l) 1)))))


; LEXER METHODS
(define (lexer-read-char l)
  (define read-pos (lexer-read-pos l))
  (if (>= read-pos (string-length (lexer-input l)))
      (set-char! l 0)
      (set-char! l (lexer-byte l)))
  (set-pos! l read-pos)
  (inc-read-pos! l))

(define (lexer-skip-whitespace l)
  (if (or
       (char-eq? (lexer-char l) " ")
       (char-eq? (lexer-char l) "\t")
       (char-eq? (lexer-char l) "\n")
       (char-eq? (lexer-char l) "\r"))
      (begin (lexer-read-char l) (lexer-skip-whitespace l))
      '()))

(define (lexer-read-identifier l)
  (define pos (lexer-pos l))
  (define (inner l)
    (if (letter? (lexer-char l))
        (begin (lexer-read-char l)
        (inner l)) '()))
  
  (inner l)
  (substring (lexer-input l) pos (lexer-pos l)))


(define (lexer-read-integer l)
  (define pos (lexer-pos l))
  (define (inner l)
    (if (digit? (lexer-char l))
        (begin (lexer-read-char l)
               (inner l)) '()))
  (inner l)
  (substring (lexer-input l) pos (lexer-pos l)))

(define (lexer-read-string l)
  (define pos (+ (lexer-pos l) 1))
  (define (inner)
    (lexer-read-char l)
    (cond
      ((char-eq? (lexer-char l) "\"") (if (char-eq? (lexer-peak-back-char l) "\\") (inner) '()))
      ((char-eq? (lexer-char l) 0) (error "EOF reached before string ended!"))
      (else (inner))))
  (inner)
  (string-replace (substring (lexer-input l) pos (lexer-pos l)) "\\\"" "\""))
