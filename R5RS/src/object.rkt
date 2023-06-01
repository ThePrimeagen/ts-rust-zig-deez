(load "../src/utils.rkt")


; TYPES
(define INTEGER_OBJ "INTEGER")
(define BOOLEAN_OBJ "BOOLEAN")
(define NULL_OBJ    "NULL")


; OBJ METHODS
(define (inspect obj)
  (define value (obj-value obj))
  (cond

    ((obj-int? obj) (number->string value))
    ((obj-bool? obj) (if value "true" "false"))
    ((obj-null? obj) "null")

    (else "Could not convert to string")
    )
  )

(define (obj-value obj)
  (caddr obj))

(define (obj-type obj)
  (if (pair? obj) (cadr obj) "EMPTY"))




; INTEGER
(define (new-int value)
  (list 'object-integer INTEGER_OBJ value))

(define (obj-int? obj)
  (tagged-list? obj 'object-integer))


; BOOLEAN
(define (new-bool-obj value)
  (list 'object-bool BOOLEAN_OBJ value))

(define THE_TRUE (new-bool-obj #t))
(define THE_FALSE (new-bool-obj #f))

(define (new-bool-obj-from-native value)
  (if value THE_TRUE THE_FALSE))

(define (obj-bool? obj)
  (tagged-list? obj 'object-bool))


; NULL
(define (new-null)
  (list 'object-null NULL_OBJ '()))

(define THE_NULL (new-null))

(define (obj-null? obj)
  (tagged-list? obj 'object-null))