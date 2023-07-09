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
- [ ] use object/is? and token/is? whereever possible.
- [ ] add constants for NULL, TRUE and FALSE in eval.clj
- [ ] Add support for
  - [ ] while loop
  - [ ] null 
  - [ ] dot property accessor for hash
  - [ ] index assignement
  - [ ] import module
