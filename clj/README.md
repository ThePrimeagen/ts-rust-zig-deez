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
- [ ] Add support for
  - [ ] while loop
  - [ ] null 
  - [ ] dot property accessor for hash
  - [ ] index assignement
  - [ ] import module
  - [ ] functions with no return statement should return null and if should should always return the last statement. (hint: compare scopes for it.)
  - [ ] try print statement after the error message when eval/run errs.
