(load "../src/utils.rkt")


; TYPES
(define INTEGER_OBJ      "INTEGER")
(define BOOLEAN_OBJ      "BOOLEAN")
(define NULL_OBJ         "NULL")
(define RETURN_VALUE_OBJ "RETURN_VALUE")
(define ERROR_OBJ        "ERROR")
(define FUNCTIONS_OBJ    "FUNCTION")


; OBJ METHODS
(define (inspect obj)
  (if (not (null? obj))
  (let* ((value (obj-value obj)))
  (cond
    ((obj-int? obj) (number->string value))
    ((obj-bool? obj) (if value "true" "false"))
    ((obj-null? obj) "null")
    ((obj-return-value? obj) "return value")
    ((obj-error? obj) (format "ERROR: " (obj-value obj)))
    ((obj-fn? obj) (format "fn(" (string-join ", " (map inspect (obj-fn-params obj))) ") {\n" (inspect (obj-fn-body obj)) "\n}"))

    (else (begin (display obj) "Could not convert to string so just displaying randomly"))))))

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


; RETURN VALUE
(define (new-return-value value)
  (list 'object-return-value RETURN_VALUE_OBJ value))

(define (obj-return-value? obj)
  (tagged-list? obj 'object-return-value))


; ERROR
(define (new-error err)
  (list 'object-error ERROR_OBJ err))

(define (obj-error? obj)
  (tagged-list? obj 'object-error))

(define (format-error format . objs)
  (for-each (lambda (obj) (set! format (string-append format (if (number? obj) (string (integer->char obj)) obj)))) objs)
  (new-error format))

; FUNCTION
(define (new-function params body env)
  (list 'obj-function params body env))

(define (obj-fn? obj)
  (tagged-list? obj 'obj-function))

(define (obj-fn-params fn)
  (cadr fn))

(define (obj-fn-body fn)
  (caddr fn))

(define (obj-fn-env fn)
  (cadddr fn))



; NULL
(define (new-null)
  (list 'object-null NULL_OBJ '()))

(define THE_NULL (new-null))

(define (obj-null? obj)
  (tagged-list? obj 'object-null))