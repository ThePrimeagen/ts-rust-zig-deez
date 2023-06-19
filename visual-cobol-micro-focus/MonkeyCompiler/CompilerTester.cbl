       class-id. CompilerTester.

       method-id main public static.
       local-storage section.
       procedure division.

           perform until exit
               invoke type MonkeyCompiler.repl::RunREPL(">>>")
           end-perform.

       end method.

       end class.
