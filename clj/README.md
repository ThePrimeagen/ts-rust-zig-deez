# monkey-lang in clojure 
Monkey Lang implement in clojure

## Usage
Both Babashka and Leiningen can be used run/test the monkey-lang.
Use `lein run` or `bb run` run the interpretor and `lein test` or 
`bb test` to run the tests.  

### TODO
- [x] Refactored token/* and ast/* constants
- [x] refactor eval/run function
- [x] refactor object/error call in eval.clj
- [x] use object/is? and token/is? whereever possible.
- [x] add constants for NULL, TRUE and FALSE in eval.clj
- [x] tail call optimization
- [x] improve lexer
- [x] add make file and update docker image to support babashka instead of leiningen
- [ ] try print statement after the error message when eval/run errs.
- [ ] input validation for builtin functions to avoid crashes
- [ ] remove (comment) from the files
- [ ] Add support for
  - [x] null 
  - [ ] variables reassignment
  - [ ] index re-assignment
  - [ ] dot property accessor for hash
  - [ ] dot property reassignment
  - [ ] while loop with continue and break
  - [ ] for loop with continue and break
  - [ ] import module
  - [ ] more operators like %, bit operations and so on
  - [ ] implement go like channels
  - [ ] add push! builtin function
  - [ ] add character datatype
