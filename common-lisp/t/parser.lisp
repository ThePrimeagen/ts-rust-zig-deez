(defpackage #:deez/test/parser
  (:use #:cl #:fiveam #:deez/parser)
  (:local-nicknames (#:rt #:deez/runtime)
                    (#:u #:deez/user))
  (:import-from #:deez/test/all
                #:deez))
(in-package #:deez/test/parser)

(def-suite deez/parser
  :description "Tests for the parser"
  :in deez)
(in-suite deez/parser)

(def-test return-statements ()
  (is-true (equal (parse-from-string "
return 5;
return 10;
return 993322;")
                  '(progn
                    (rt:|return| 5)
                    (rt:|return| 10)
                    (rt:|return| 993322)))))

(def-test identifier-expression ()
  (is-true (eq (parse-from-string "foobar;")
               'u::|foobar|)))

(def-test integer-literal ()
  (is-true (= (parse-from-string "5;") 5)))

(def-test infix-expression ()
  (loop for (code expected) in '(("5 + 5"
                                  (rt:+ 5 5))
                                 ("5 - 5"
                                  (rt:- 5 5))
                                 ("5 * 5"
                                  (rt:* 5 5))
                                 ("5 / 5"
                                  (rt:/ 5 5))
                                 ("5 > 5"
                                  (rt:> 5 5))
                                 ("5 < 5"
                                  (rt:< 5 5))
                                 ("5 == 5"
                                  (rt:== 5 5))
                                 ("5 != 5"
                                  (rt:!= 5 5))
                                 ("true == true"
                                  (rt:== rt:|true| rt:|true|))
                                 ("true != false"
                                  (rt:!= rt:|true| rt:|false|))
                                 ("false == false"
                                  (rt:== rt:|false| rt:|false|)))
        do (is-true (equal (parse-from-string code)
                           expected))))

(def-test prefix-expression ()
  (loop for (code expected) in '(("!5;"
                                  (rt:! 5))
                                 ("-15"
                                  (rt:- 15))
                                 ("!true;"
                                  (rt:! rt:|true|))
                                 ("!false;"
                                  (rt:! rt:|false|)))
        do (is-true (equal (parse-from-string code)
                           expected))))

(def-test if-expression ()
  (let* ((code "if (x < y) { x }")
         (ast (parse-from-string code))
         (test (cadr ast))
         (then (caddr ast))
         (else (cadddr ast)))
    (is-true (eq (car ast) 'rt:|if|))
    (is-true (equal test '(rt:< u::|x| u::|y|)))
    (is-true (equal then '(progn u::|x|)))
    (is-true (null else))))

(def-test function-literal ()
  (let* ((code "fn(x, y) { x + y; }")
         (ast (parse-from-string code))
         (parameters (cadr ast))
         (body (caddr ast))
         (statements (cdaddr (caddr body))))
    (is-true (eq (car ast) 'lambda))
    (is-true (equal parameters '(u::|x| u::|y|)))
    (is-true (= (length statements) 1))
    (is-true (equal (car statements) '(rt:+ u::|x| u::|y|)))))

(def-test function-parameter ()
  (loop for (code expected) in '(("fn() {};" nil)
                                 ("fn(x) {};" (u::|x|))
                                 ("fn(x, y, z) {};" (u::|x| u::|y| u::|z|)))
        for ast = (parse-from-string code)
        do (is-true (equal (cadr ast) expected))))

(def-test call-expression ()
  (is-true (equal (parse-from-string "add(1, 2 * 3, 4 + 5);")
                  '(funcall u::|add| 1 (rt:* 2 3) (rt:+ 4 5)))))

(def-test operator-precedence ()
  (loop for (code expected) in '(("-a * b"
                                  (rt:* (rt:- u::|a|) u::|b|))
                                 ("!-a"
                                  (rt:! (rt:- u::|a|)))
                                 ("a + b + c"
                                  (rt:+ (rt:+ u::|a| u::|b|) u::|c|))
                                 ("a + b - c"
                                  (rt:- (rt:+ u::|a| u::|b|) u::|c|))
                                 ("a * b * c"
                                  (rt:* (rt:* u::|a| u::|b|) u::|c|))
                                 ("a * b / c"
                                  (rt:/ (rt:* u::|a| u::|b|) u::|c|))
                                 ("a + b / c"
                                  (rt:+ u::|a| (rt:/ u::|b| u::|c|)))
                                 ("a + b * c + d / e - f"
                                  (rt:- (rt:+ (rt:+ u::|a| (rt:* u::|b| u::|c|)) (rt:/ u::|d| u::|e|)) u::|f|))
                                 ("3 + 4; -5 * 5"
                                  (progn (rt:+ 3 4)
                                         (rt:* (rt:- 5) 5)))
                                 ("5 > 4 == 3 < 4"
                                  (rt:== (rt:> 5 4) (rt:< 3 4)))
                                 ("5 < 4 != 3 > 4"
                                  (rt:!= (rt:< 5 4) (rt:> 3 4)))
                                 ("3 + 4 * 5 == 3 * 1 + 4 * 5"
                                  (rt:== (rt:+ 3 (rt:* 4 5)) (rt:+ (rt:* 3 1) (rt:* 4 5))))
                                 ("true"
                                  rt:|true|)
                                 ("false"
                                  rt:|false|)
                                 ("3 > 5 == false"
                                  (rt:== (rt:> 3 5) rt:|false|))
                                 ("3 < 5 == true"
                                  (rt:== (rt:< 3 5) rt:|true|))
                                 ("(5 + 5) * 2"
                                  (rt:* (rt:+ 5 5) 2))
                                 ("2 / (5 + 5)"
                                  (rt:/ 2 (rt:+ 5 5)))
                                 ("-(5 + 5)"
                                  (rt:- (rt:+ 5 5)))
                                 ("!(true == true)"
                                  (rt:! (rt:== rt:|true| rt:|true|)))
                                 ("a + add(b * c) + d"
                                  (rt:+ (rt:+ u::|a| (funcall u::|add| (rt:* u::|b| u::|c|))) u::|d|))
                                 ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))"
                                  (funcall u::|add| u::|a| u::|b| 1 (rt:* 2 3) (rt:+ 4 5) (funcall u::|add| 6 (rt:* 7 8))))
                                 ("add(a + b + c * d / f + g)"
                                  (funcall u::|add| (rt:+ (rt:+ (rt:+ u::|a| u::|b|) (rt:/ (rt:* u::|c| u::|d|) u::|f|)) u::|g|))))
        do (is-true (equal (parse-from-string code) expected))))

(def-test let-statements ()
  (loop for (code expected-sym expected-value) in '(("let x = 5;" u::|x| 5)
                                                    ("let y = true;" u::|y| rt:|true|)
                                                    ("let foobar = y;" u::|foobar| u::|y|))
        for ast = (parse-from-string code)
        do (progn
             (is-true (eq (car ast) 'defparameter))
             (is-true (eq (cadr ast) expected-sym))
             (is-true (eq (caddr ast) expected-value)))))
