(load "utils.rkt")

(define (new-lexer input)
  (define l (list "LEXER" input 0 0 0))
  (lexer-read-char l)
  l
  )

(define (lexer? token)
  (if (pair? token)
      (eq? (car token) "LEXER")
      #f))

(define (lexer-input l)
  (if (lexer? l)
      (cadr l)
      '()))

(define (lexer-byte l)
  (char->integer (string-ref (lexer-input l) (lexer-read-pos l)))
  )

(define (set-input! l input)
  (set-car! (cdr l) input)
)

(define (lexer-pos l)
  (if (lexer? l)
      (caddr l)
      '()))

(define (set-pos! l pos)
  (set-car! (cddr l) pos)
)

(define (lexer-read-pos l)
  (if (lexer? l)
      (cadddr l)
      '()))

(define (set-read-pos! l read-pos)
  (set-car! (cdddr l) read-pos)
)

(define (inc-read-pos! l)
  (set-read-pos! l (+ (lexer-read-pos l) 1)))

(define (lexer-char l)
  (if (lexer? l)
      (cadr (cdddr l))
      '()))

(define (set-char! l char)
  (set-car! (cdr (cdddr l)) char)
)

(define (lexer-skip-whitespace l)
  (if (or
       (char-eq? (lexer-char l) " ")
       (char-eq? (lexer-char l) "\t")
       (char-eq? (lexer-char l) "\n")
       (char-eq? (lexer-char l) "\r"))
      (begin (lexer-read-char l) (lexer-skip-whitespace l))
      '()))

(define (lexer-read-char l)
  (define read-pos (lexer-read-pos l))
  (if (>= read-pos (string-length (lexer-input l)))
      (set-char! l 0)
      (set-char! l (lexer-byte l))
  )
  (set-pos! l read-pos)
  (inc-read-pos! l)
  )

(define (lexer-peak-char l)
  (if (>= (lexer-read-pos l) (string-length (lexer-input l)))
      0
      (lexer-byte l)
      )
  )


(define (lexer-read-identifier l)
  (define pos (lexer-pos l))
  (define (inner l)
    (if (letter? (lexer-char l))
        (begin (lexer-read-char l)
        (inner l))
        '()
        ))
  
  (inner l)
  (substring (lexer-input l) pos (lexer-pos l))
  )

(define (lexer-read-integer l)
  (define pos (lexer-pos l))
  (define (inner l)
    (if (digit? (lexer-char l))
        (begin (lexer-read-char l)
               (inner l))
        '()
        ))
  (inner l)
  (substring (lexer-input l) pos (lexer-pos l))
  )
  

  











 