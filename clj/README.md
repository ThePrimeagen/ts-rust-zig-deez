# monkey-lang in clojure 
Monkey Lang implement in clojure

## Commands
- ### Run
  ```console
  $ bb run
  ``` 
  or  
  ```console
  $ lein run
  ```  
- ### Test
  ```console
  $ bb test
  ```  
  or  
  ```console
  $ lein test
  ```  

### TODO
- [x] Refactored token/* and ast/* constants
- [ ] use object/is? and token/is? whereever possible.
- [ ] refactor object/error call in eval.clj
- [ ] refactor eval/run function
- [ ] use token/* const wherever possible
- [ ] add constants for NULL, TRUE and FALSE in eval.clj
- [ ] Add support for
  - [ ] while loop
  - [ ] null 
  - [ ] dot property accessor for hash
  - [ ] index assignement
  - [ ] import module
