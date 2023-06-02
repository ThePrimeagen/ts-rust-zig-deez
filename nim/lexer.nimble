# Package

version       = "0.1.0"
author        = "Echriser"
description   = "A new awesome nimble package"
license       = "MIT"
srcDir        = "src"
bin           = @["lexer"]


# Dependencies

requires "nim >= 1.6.12"

task test, "Run lexer tests":
  withDir "tests":
    exec "nim c -r test"
