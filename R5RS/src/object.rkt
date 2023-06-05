(load "../src/utils.rkt")


; HASHER
(define HASHER-CACHE (make-hash))

(define (make-hash-key obj)
  (cond
    ((obj-bool? obj) (list 'hash-key (obj-type obj) (if (obj-value obj) 1 0)))
    ((obj-int? obj) (list 'hash-key (obj-type obj) (obj-value obj)))
    ((obj-string? obj) (begin
                         (let* ((val (obj-value obj))
                                (hash-check (hash-ref HASHER-CACHE val '())))
                           (if (not (null? hash-check)) hash-check
                               (let* ((hash-value (fnv-hash (obj-value obj)))
                                      (hash-key (list 'hash-key (obj-type obj) hash-value)))
                                 (begin (hash-set! HASHER-CACHE val hash-key) hash-key))))))))

(define (hash-key? key)
  (tagged-list? key 'hash-key))

(define (can-hash? obj)
  (or (obj-bool? obj) (obj-int? obj) (obj-string? obj)))


; TYPES
(define INTEGER_OBJ      "INTEGER")
(define BOOLEAN_OBJ      "BOOLEAN")
(define NULL_OBJ         "NULL")
(define RETURN_VALUE_OBJ "RETURN_VALUE")
(define ERROR_OBJ        "ERROR")
(define FUNCTIONS_OBJ    "FUNCTION")
(define STRING_OBJ       "STRING")
(define BUILDIN_OBJ      "BUILDIN")
(define ARRAY_OBJ        "ARRAY")
(define HASH_OBJ         "HASH")


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
    ((obj-string? obj) (string-value obj))
    ((obj-array? obj) (format "[" (string-join ", " (map inspect (obj-value obj))) "]"))

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


; STRING
(define (new-string-obj value)
  (list 'object-string STRING_OBJ value))

(define (obj-string? obj)
  (tagged-list? obj 'object-string))


; ARRAY
(define (new-array-obj el)
  (list 'object-array ARRAY_OBJ el))

(define (obj-array? obj)
  (tagged-list? obj 'object-array))


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
  (list 'obj-function FUNCTIONS_OBJ params body env))

(define (obj-fn? obj)
  (tagged-list? obj 'obj-function))

(define (obj-fn-params fn)
  (caddr fn))

(define (obj-fn-body fn)
  (cadddr fn))

(define (obj-fn-env fn)
  (cadr (cdddr fn)))


; BUILDIN FUNCTION
(define (new-buildin function)
  (list 'obj-buildin BUILDIN_OBJ function))

(define (obj-buildin? obj)
  (tagged-list? obj 'obj-buildin))

(define (buildin-function buildin)
  (caddr buildin))


; HASH
(define (new-hash-obj hash)
  (list 'obj-hash HASH_OBJ hash))

(define (obj-hash? obj)
  (tagged-list? obj 'obj-hash))

(define (hash-obj-hash hash)
  (caddr hash))

(define (hash-obj-set! h key value)
  (hash-set! (hash-obj-hash h) key value))

(define (hash-obj-ref h key)
  (hash-ref (hash-obj-hash h) key '()))

; HASH PAIR
(define (new-hash-pair key value)
  (list 'hash-pair key value))

(define (hash-pair? pair)
  (tagged-list? pair 'hash-pair))

(define (hash-pair-key pair)
  (cadr pair))

(define (hash-pair-value pair)
  (caddr pair))


; NULL
(define (new-null)
  (list 'object-null NULL_OBJ '()))

(define THE_NULL (new-null))

(define (obj-null? obj)
  (tagged-list? obj 'object-null))