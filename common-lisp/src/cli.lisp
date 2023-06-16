(defpackage #:deez/cli
  (:use #:cl #:deez/parser #:deez/lexer)
  (:import-from #:unix-opts)
  (:export
   #:main))
(in-package #:deez/cli)

(opts:define-opts
  (:name :help
   :description "Prints this help."
   :short #\h
   :long "help")
  (:name :time
   :description "Instead of printing results, print time."
   :short #\t
   :long "time")
  (:name :lex
   :description "Only lex."
   :short #\l
   :long "lex")
  (:name :parse
   :description "Only parse."
   :short #\p
   :long "parse")
  (:name :output
   :description "Create executable at FILE."
   :short #\o
   :long "output"
   :arg-parser #'identity
   :meta-var "FILE"))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-case
          (opts:get-opts)
        (opts:missing-arg (condition)
          (format t "Option ~A requires an argument.~%"
                  (opts:option condition))))
    (destructuring-bind (&key help time lex parse output)
        options
      (when help
        (opts:describe
         :usage-of "cl-deez"
         :args "[FILE]")
        (uiop:quit))
      (unless (= (length free-args) 1)
        (format t "Please provide a filename to run.~%")
        (opts:exit 1))
      (with-open-file (file (car free-args))
        (let ((*standard-input* file))
          (cond
            ((and lex time)
             (time (loop for token = (lex)
                         while token)))
            ((and parse time)
             (time (parse)))
            (lex
             (format t "~S~%" (lex-to-eof)))
            (parse
             (format t "~S~%" (parse)))
            (output
             ;; TODO: Not sure how to dump this with uiop without super weird things happening
             ;; TODO: Can we get rid of the warnings this prints for let?
             (sb-ext:save-lisp-and-die output
                                       :executable t
                                       :compression t
                                       :toplevel (compile nil `(lambda () ,(parse)))))
            (t (eval (parse)))))))))
