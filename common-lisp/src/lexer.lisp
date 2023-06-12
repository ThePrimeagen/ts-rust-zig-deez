(defpackage #:deez/lexer
  (:use #:cl)
  (:import-from #:deez/user)
  (:export
   #:lex
   #:lex-to-eof
   #:lex-from-string
   #:comment))
(in-package #:deez/lexer)

(defparameter *single-char-tokens*
  '(#\( #\)
    #\[ #\]
    #\{ #\}
    #\; #\, #\: #\=))

(defparameter *user-package* (find-package '#:deez/user))

(defmacro read-while ((char-symbol) &body test)
  (let ((output (gensym "OUTPUT")))
    `(with-output-to-string (,output)
       (loop for ,char-symbol = (peek-char nil nil nil)
             while (and ,char-symbol (progn ,@test))
             do (write-char (read-char) ,output)))))

(defun read-integer ()
  (read-while (char)
    (digit-char-p char)))

(defun read-symbol ()
  (read-while (char)
    (or (alpha-char-p char)
        (char= char #\_))))

(defun peek-2 ()
  (let ((c (read-char)))
    (prog1
        (peek-char nil nil nil)
      (unread-char c))))

(defun char-2= (char equal-to peek-2-equal-to)
  "Like CHAR= but peeks another char and compares it."
  (and (char= char equal-to)
       (char= (or (peek-2) #\Nul) peek-2-equal-to)))

(defun lex ()
  (let ((c (peek-char t nil nil)))
    (cond
      ((null c) nil)
      ((digit-char-p c)
       (parse-integer (read-integer)))
      ((char-2= c #\= #\=)
       (read-char)
       (read-char)
       'deez/runtime:==)
      ((char-2= c #\! #\=)
       (read-char)
       (read-char)
       'deez/runtime:!=)
      ((and (char= c #\/)
            (char= (peek-2) #\/))
       (read-line)
       (lex))
      ((member c *single-char-tokens*)
       (read-char))
      ((char= c #\!) (prog1 'deez/runtime:! (read-char)))
      ((char= c #\+) (prog1 'deez/runtime:+ (read-char)))
      ((char= c #\-) (prog1 'deez/runtime:- (read-char)))
      ((char= c #\*) (prog1 'deez/runtime:* (read-char)))
      ((char= c #\/) (prog1 'deez/runtime:/ (read-char)))
      ((char= c #\<) (prog1 'deez/runtime:< (read-char)))
      ((char= c #\>) (prog1 'deez/runtime:> (read-char)))
      ((char= c #\")
       (read))
      ((or (alpha-char-p c)
           (char= c #\_))
       (let ((*package* *user-package*))
         (intern (read-symbol))))
      (t (error "Syntax error")))))

(defun lex-to-eof ()
  (loop for token = (lex)
        while token
        collect token))

(defun lex-from-string (string)
  (with-input-from-string (*standard-input* string)
    (lex-to-eof)))
