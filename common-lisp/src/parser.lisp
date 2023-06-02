(defpackage #:deez/parser
  (:use #:cl)
  (:import-from #:deez/user)
  (:export
   #:parse
   #:parse-from-string))
(in-package #:deez/parser)

(defvar *runtime-package* (find-package '#:deez/runtime))

(defparameter *lookahead* nil)

(defun lex ()
  (if *lookahead*
      (prog1
          *lookahead*
        (setf *lookahead* nil))
      (deez/lexer:lex)))

(defun lex-peek ()
  (or *lookahead*
      (setf *lookahead* (deez/lexer:lex))))

(defun ensure-next (what)
  (let ((next (lex)))
    (unless (eql next what)
      (error "Syntax error, expected ~A but got ~A" what next))))

(defun ensure-not-reserved (symbol)
  (when (eq (symbol-package symbol) *runtime-package*)
    (error "Syntax error, ~A is a reserved keyword" symbol)))

(let ((table '((deez/runtime:== . 1)
               (deez/runtime:!= . 1)
               (deez/runtime:< . 2)
               (deez/runtime:> . 2)
               (deez/runtime:+ . 3)
               (deez/runtime:- . 3)
               (deez/runtime:* . 4)
               (deez/runtime:/ . 5)
               (:prefix . 6)
               (#\( . 7)
               (#\[ . 7))))
  (defun precedence (token)
    (cdr (assoc token table))))

(defun parse-let (toplevel-p)
  (lex)
  (let* ((sym (lex))
         (next (lex))
         (initial-value (cond
                          ((eq next #\;) nil)
                          ((eq next #\=)
                           (prog1
                               (parse-expression)
                             (ensure-next #\;)))
                          (t (error "Syntax error, expected ; or = but got ~A" next)))))
    (ensure-not-reserved sym)
    (if toplevel-p
        (values `(defparameter ,sym ,initial-value) nil)
        (values `(let ((,sym ,initial-value))) t))))

(defun parse-arglist ()
  (when (eql (lex-peek) #\))
    (lex)
    (return-from parse-arglist nil))
  (loop for arg = (lex)
        for next = (lex)
        collect arg
        while (and arg (not (eql next #\))))
        unless (eql next #\,)
          do (error "Syntax error, expected , but got ~A" next)))

(defun parse-fn ()
  (ensure-next #\()
  (let ((args (parse-arglist))
        (body (parse-block)))
    (loop for arg in args
          unless (typep arg 'symbol)
            do (error "Syntax error, function arguments can only be symbols, got ~A" arg)
          do (ensure-not-reserved arg))
    (let ((return-label (gensym "RETURN")))
      `(lambda ,args
         (macrolet ((deez/runtime:|return| (form)
                      `(return-from ,',return-label ,form)))
           (block ,return-label
             ,body))))))

(defun parse-if ()
  (ensure-next #\()
  (let ((test (prog1
                  (parse-expression)
                (ensure-next #\))))
        (then (parse-block))
        (else (when (eq (lex-peek) 'deez/runtime:|else|)
                (lex)
                (parse-block))))
    `(deez/runtime:|if| ,test
                   ,then
                   ,else)))

(defun parse-block ()
  (ensure-next #\{)
  (cons
   'progn
   (loop for token = (lex-peek)
         unless token
           do (error "Syntax error, expected } got EOF")
         until (eql token #\})
         unless (eql token #\;)
           collect (parse-statement)
         finally (lex))))

(defun parse-array ()
  (loop
    with array = (make-array 0 :adjustable t :fill-pointer t)
    for elem = (parse-expression)
    for next = (lex)
    do (vector-push-extend elem array)
    while (and next
               (not (eql next #\])))
    unless (eql next #\,)
      do (error "Syntax error, expected , but got ~A" next)
    finally (return array)))

(defun parse-funcall (symbol)
  `(funcall ,symbol ,@(parse-arglist)))

(defun parse-array/hash-access (symbol)
  (lex)
  (prog1
      `(deez/runtime::array/hash-access ,symbol ,(parse-expression))
    (ensure-next #\])))

(defun parse-group ()
  (prog1
      (parse-expression)
    (ensure-next #\))))

(defun parse-statement (&optional toplevel-p)
  (let ((token (lex-peek)))
    (cond
      ((null token)
       (error "Unexpected end of input"))
      ((eq token 'deez/runtime:|let|)
       (parse-let toplevel-p))
      ((eq token 'deez/runtime:|return|)
       (lex)
       (prog1
           (list token (parse-expression 0))
         (ensure-next #\;)))
      (t (prog1
             (parse-expression 0)
           (when (eql (lex-peek) #\;)
             (lex)))))))

(defun parse-expression (&optional (precedence 0))
  (labels ((parse-single ()
             (let ((token (lex)))
               (typecase token
                 (null (error "Unexpected end of input"))
                 (symbol
                  (cond
                    ((eq token 'deez/runtime:|fn|)
                     (parse-fn))
                    ((eq token 'deez/runtime:|if|)
                     (parse-if))
                    ((or (eq token 'deez/runtime:!)
                         (eq token 'deez/runtime:-))
                     (list token (parse-expression (precedence :prefix))))
                    (t token)))
                 (character
                  (cond
                    ((eql token #\()
                     (parse-group))
                    ((eql token #\[)
                     (parse-array))
                    (t (error "Syntax error, unexpected ~A" token))))
                 (atom token)
                 (t (error "Expected known token type, got ~A" (type-of token)))))))
    (loop with expr = (parse-single)
          for op = (lex-peek)
          for next-precedence = (and op (precedence op))
          while (and next-precedence
                     (< precedence next-precedence))
          do (lex)
          do (setf expr
                   (cond
                     ((eql op #\()
                      (parse-funcall expr))
                     ((eql op #\[)
                      (parse-array/hash-access expr))
                     (t (list op expr (parse-expression next-precedence)))))
          finally (return expr))))

(defun %parse (&optional toplevel-p)
  (loop
    with forms = (list 'progn)
    with head = forms
    for token = (lex-peek)
    while token
    do (multiple-value-bind (next replace-p)
           (parse-statement toplevel-p)
         (push next forms)
         (if replace-p
             (setf forms next)
             (setf head forms)))
    finally (return (let ((forms (nreverse head)))
                      (if (cddr forms)
                          forms
                          (cadr forms))))))

(defun parse ()
  (let ((*lookahead* nil))
    (%parse t)))

(defun parse-from-string (string)
  (with-input-from-string (*standard-input* string)
    (parse)))
