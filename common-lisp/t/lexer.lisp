(defpackage #:deez/test/lexer
  (:use #:cl #:fiveam #:deez/lexer)
  (:import-from #:deez/test/all
                #:deez))
(in-package #:deez/test/lexer)

(def-suite deez/lexer
  :description "Tests for the lexer"
  :in deez)
(in-suite deez/lexer)

(def-test next-token ()
  (macrolet ((next-is (expected)
               `(is-true (equal (lex) ,expected)))
             (test-all (list)
               `(loop for item in ,list
                      do (next-is item))))
    (with-input-from-string (*standard-input* "let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[1, 2];
{\"foo\": \"bar\"}
")
      (test-all '(deez/runtime:|let| deez/user::|five| #\= 5 #\;
                  deez/runtime:|let| deez/user::|ten| #\= 10 #\;
                  deez/runtime:|let| deez/user::|add| #\= deez/runtime:|fn| #\( deez/user::|x| #\, deez/user::|y| #\) #\{
                  deez/user::|x| deez/runtime:+ deez/user::|y| #\;
                  #\} #\;
                  deez/runtime:|let| deez/user::|result| #\= deez/user::|add| #\( deez/user::|five| #\, deez/user::|ten| #\) #\;
                  deez/runtime:! deez/runtime:- deez/runtime:/ deez/runtime:* 5 #\;
                  5 deez/runtime:< 10 deez/runtime:> 5 #\;
                  deez/runtime:|if| #\( 5 deez/runtime:< 10 #\) #\{
                  deez/runtime:|return| deez/runtime:|true| #\;
                  #\} deez/runtime:|else| #\{
                  deez/runtime:|return| deez/runtime:|false| #\;
                  #\}
                  10 deez/runtime:|==| 10 #\;
                  10 deez/runtime:|!=| 9 #\;
                  "foobar"
                  "foo bar"
                  #\[ 1 #\, 2 #\] #\;
                  #\{ "foo" #\: "bar" #\})))))

(def-test variable-definitions ()
  (loop for (code expected-tokens) in '(("let version = 1;"
                                         (deez/runtime:|let| deez/user::|version| #\= 1 #\;))
                                        ("let name = \"Monkey programming language\";"
                                         (deez/runtime:|let| deez/user::|name| #\= "Monkey programming language" #\;))
                                        ("let myArray = [1, 2, 3, 4, 5];"
                                         (deez/runtime:|let| deez/user::|myArray| #\= #\[ 1 #\, 2 #\, 3 #\, 4 #\, 5 #\] #\;))
                                        ("let coolBooleanLiteral = true;"
                                         (deez/runtime:|let| deez/user::|coolBooleanLiteral| #\= deez/runtime:|true| #\;))
                                        ("let awesomeValue = (10 / 2) * 5 + 30;"
                                         (deez/runtime:|let| deez/user::|awesomeValue| #\= #\( 10 deez/runtime:/ 2 #\) deez/runtime:* 5 deez/runtime:+ 30 #\;))
                                        ("let arrayWithValues = [1 + 1, 2 * 2, 3];"
                                         (deez/runtime:|let| deez/user::|arrayWithValues| #\= #\[ 1 deez/runtime:+ 1 #\, 2 deez/runtime:* 2 #\, 3 #\] #\;))
                                        ("let people = [{\"name\": \"Anna\", \"age\": 24}, {\"name\": \"Bob\", \"age\": 99}];"
                                         (deez/runtime:|let| deez/user::|people| #\= #\[
                                          #\{ "name" #\: "Anna" #\, "age" #\: 24 #\} #\,
                                          #\{ "name" #\: "Bob" #\, "age" #\: 99 #\} #\] #\;)))
        for tokens = (lex-from-string code)
        do (is-true (equal tokens expected-tokens))))

(def-test fibonacci ()
  (is-true (equal (lex-from-string "
// Define a `fibonacci` function
let fibonacci = fn(x) {
  if (x == 0) {
    0                // Monkey supports implicit returning of values
  } else {
    if (x == 1) {
      return 1;      // ... and explicit return statements
    } else {
      fibonacci(x - 1) + fibonacci(x - 2); // Recursion! Yay!
    }
  }
};")
                  '(deez/runtime:|let| deez/user::|fibonacci| #\= deez/runtime:|fn| #\( deez/user::|x| #\) #\{
                    deez/runtime:|if| #\( deez/user::|x| deez/runtime:|==| 0 #\) #\{
                    0
                    #\} deez/runtime:|else| #\{
                    deez/runtime:|if| #\( deez/user::|x| deez/runtime:|==| 1 #\) #\{
                    deez/runtime:|return| 1 #\;
                    #\} deez/runtime:|else| #\{
                    deez/user::|fibonacci| #\( deez/user::|x| deez/runtime:- 1 #\) deez/runtime:+ deez/user::|fibonacci| #\( deez/user::|x| deez/runtime:- 2 #\) #\;
                    #\}
                    #\}
                    #\} #\;))))

(def-test map ()
  (is-true (equal (lex-from-string "
// Define the higher-order function `map`, that calls the given function `f`
// on each element in `arr` and returns an array of the produced values.
let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))));
    }
  };

  iter(arr, []);
};

// Now let's take the `people` array and the `getName` function from above and
// use them with `map`.
map(people, getName); // => [\"Anna\", \"Bob\"]")
                  '(deez/runtime:|let| deez/user::|map| #\= deez/runtime:|fn| #\( deez/user::|arr| #\, deez/user::|f| #\) #\{
                    deez/runtime:|let| deez/user::|iter| #\= deez/runtime:|fn| #\( deez/user::|arr| #\, deez/user::|accumulated| #\) #\{
                    deez/runtime:|if| #\( deez/runtime:|len| #\( deez/user::|arr| #\) deez/runtime:== 0 #\) #\{
                    deez/user::|accumulated|
                    #\} deez/runtime:|else| #\{
                    deez/user::|iter| #\( deez/runtime:|rest| #\( deez/user::|arr| #\) #\, deez/runtime:|push| #\( deez/user::|accumulated| #\, deez/user::|f| #\( deez/runtime:|first| #\( deez/user::|arr| #\) #\) #\) #\) #\;
                    #\}
                    #\} #\;

                    deez/user::|iter| #\( deez/user::|arr| #\, #\[ #\] #\) #\;
                    #\} #\;

                    deez/user::|map| #\( deez/user::|people| #\, deez/user::|getName| #\) #\;))))
