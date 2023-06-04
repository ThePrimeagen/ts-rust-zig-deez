(load "../src/utils.rkt")

; ENVIRONMENT
(define (new-environment)
  (list 'environment (make-hash) '()))

(define (is-environment? e)
  (tagged-list? e 'environment))

(define (get-from-environment e key)
  (let* ((val (hash-ref (cadr e) key
                        (lambda ()
                          (if (null? (environment-outer e)) '() (get-from-environment (environment-outer e) key))
                          )))) val))

(define (add-to-environment e key val)
  (hash-set! (cadr e) key val)
  val)

(define (environment-outer env)
  (caddr env))

(define (new-environment-with-outer outer)
  (list 'environment (make-hash) outer))


; ENCLOSED ENVIRONMENT
(define (new-enclosed-environment outer)
  (new-environment-with-outer outer))
  