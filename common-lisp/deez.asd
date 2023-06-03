(defsystem "deez"
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("deez/lexer"
               "deez/parser")
  :in-order-to ((test-op (load-op "deez/test")))
  :perform (test-op (o c) (symbol-call :deez/test/all :run-tests)))

(defsystem "deez/executable"
  :depends-on ("deez" "deez/cli")
  :build-operation program-op
  :build-pathname "cl-deez"
  :entry-point "deez/cli:main")

(defsystem "deez/test"
  :pathname "t"
  :serial t
  :depends-on ("fiveam")
  :components ((:file "all")
               (:file "lexer")
               (:file "parser")))
