USING: kernel prettyprint arrays strings math locals accessors 
    sequences combinators namespaces vectors assocs ;
IN: deez.lexer


SYMBOL: tkIllegal
SYMBOL: tkEOF
SYMBOL: tkIdent
SYMBOL: tkInt
INITIALIZED-SYMBOL: tkAssign    [ "=" ]
INITIALIZED-SYMBOL: tkEq        [ "==" ]
INITIALIZED-SYMBOL: tkNEq       [ "!=" ]
INITIALIZED-SYMBOL: tkPlus      [ "+" ]
INITIALIZED-SYMBOL: tkMinus     [ "-" ]
INITIALIZED-SYMBOL: tkBang      [ "!" ]
INITIALIZED-SYMBOL: tkStar      [ "*" ]
INITIALIZED-SYMBOL: tkSlash     [ "/" ]
INITIALIZED-SYMBOL: tkComma     [ "," ]
INITIALIZED-SYMBOL: tkLT        [ "<" ]
INITIALIZED-SYMBOL: tkGT        [ ">" ]
INITIALIZED-SYMBOL: tkLTEq      [ "<=" ]
INITIALIZED-SYMBOL: tkGTEq      [ ">=" ]
INITIALIZED-SYMBOL: tkLParen    [ "(" ]
INITIALIZED-SYMBOL: tkRParen    [ ")" ]
INITIALIZED-SYMBOL: tkLSquirly  [ "{" ]
INITIALIZED-SYMBOL: tkRSquirly  [ "}" ]
INITIALIZED-SYMBOL: tkFunction  [ "fn" ]
INITIALIZED-SYMBOL: tkLet       [ "let" ]
INITIALIZED-SYMBOL: tkTrue      [ "true" ]
INITIALIZED-SYMBOL: tkFalse     [ "false" ]
INITIALIZED-SYMBOL: tkIf        [ "if" ]
INITIALIZED-SYMBOL: tkElse      [ "else" ]
INITIALIZED-SYMBOL: tkReturn    [ "return" ]

INITIALIZED-SYMBOL: token-type-lookups [ H{ 
    { "fn"      tkFunction  }
    { "let"     tkLet       }
    { "true"    tkTrue      }
    { "false"   tkFalse     }
    { "if"      tkIf        }
    { "else"    tkElse      }
    { "return"  tkReturn    }
} ] 

: sym-char ( sym -- ch ) get 0 swap nth ;

: letter? ( c -- t/f )
    { [ CHAR: a >= ] [ CHAR: z <= and ] 
      [ CHAR: A >= ] [ CHAR: Z <= and ] 
      [ CHAR: _ = or or ] } cleave ;

: whitespace? ( c -- t/f ) 
    { [ CHAR: space = ] [ CHAR: \t = ] [ CHAR: \n = ] [ CHAR: \r = ] } 
    cleave or or or ;

: digit? ( c -- t/f ) 
    [ CHAR: 0 >= ] [ CHAR: 9 <= ] bi and ;

TUPLE: tokentype literal type ;
C: <tokentype> tokentype

TUPLE: lexer
 { text string }
 { cursor fixnum }
 { line fixnum }
 { col fixnum } ;

: <lexer> ( text -- lexer )
    0 1 1 lexer boa ;

: lexing? ( lexer -- lexer t/f )
    dup [ cursor>> ] [ text>> length ] bi < ;

: inc-cursor ( lexer -- lexer )
    dup cursor>> 1 + >>cursor ;

: dec-cursor ( lexer -- lexer )
    dup cursor>> 1 - >>cursor ;

: current-char ( lexer -- lexer ch )
    dup [ cursor>> ] [ text>> ] bi nth ;

: peek-char ( lexer -- lexer ch/f )
    dup [ cursor>> 1 + ] [ text>> ] [ text>> length ] tri
    pick > [ nth ] [ 2drop f ] if ;

: next-char ( lexer -- lexer ch )
    dup [ cursor>> ] [ text>> ] bi nth swap inc-cursor swap ;

: ?next-char ( lexer -- lexer ch/f )
    lexing? [ next-char ] [ f ] if ;

: read-identifier ( lexer ch -- lexer string ) 
    1vector swap 
    [ ?next-char dup [ dup letter? [ ] [ drop dec-cursor f ] if ] when ] 
    [ pick push ] while* swap >string ;

: read-number ( lexer ch -- lexer string )
    1vector swap
    [ ?next-char dup [ dup digit? [ ] [ drop dec-cursor f ] if ] when ] 
    [ pick push ] while* swap >string ;

: skip-whitespace ( lexer -- lexer )
    lexing? [ [ current-char whitespace? ] [ inc-cursor ] while ] when ;

: ?keyword ( string -- string kw/ident )
    dup token-type-lookups get at dup [ drop tkIdent ] unless ;


: ?next-token ( lexer -- lexer token/f )
    skip-whitespace ?next-char dup [ 
     {
        { [ dup tkAssign sym-char = ] 
          [ over current-char tkAssign sym-char
            = [ inc-cursor drop drop tkEq get tkEq ] 
              [ drop tkAssign ] if ] }
        { [ dup tkPlus sym-char = ] [ tkPlus ] }
        { [ dup tkMinus sym-char = ] [ tkMinus ] }
        { [ dup tkBang sym-char = ] 
          [ over current-char tkAssign sym-char
            = [ inc-cursor drop drop tkNEq get tkNEq ] 
              [ drop tkBang ] if ] }
        { [ dup tkStar sym-char = ] [ tkStar ] }
        { [ dup tkSlash sym-char = ] [ tkSlash ] }
        { [ dup tkComma sym-char = ] [ tkComma ] }
        { [ dup tkLT sym-char = ] 
          [ over current-char tkAssign sym-char
            = [ inc-cursor drop drop tkLTEq get tkLTEq ] 
              [ drop tkLT ] if ] }
        { [ dup tkGT sym-char = ] 
          [ over current-char tkAssign sym-char
            = [ inc-cursor drop drop tkGTEq get tkGTEq ] 
              [ drop tkGT ] if ] }
        { [ dup tkLParen sym-char = ] [ tkLParen ] }
        { [ dup tkRParen sym-char = ] [ tkRParen ] }
        { [ dup tkLSquirly sym-char = ] [ tkLSquirly ] }
        { [ dup tkRSquirly sym-char = ] [ tkRSquirly ] }
        { [ dup tkPlus sym-char = ] [ tkPlus ] }
        { [ dup letter? ] [ read-identifier ?keyword ] }
        { [ dup digit? ] [ read-number tkInt ] }
        [ tkIllegal ]
    } cond <tokentype> ] [ drop f ] if ;

: lex ( lexer -- seq )
    0 <vector> swap [ 
        ?next-token dup [ pick push t ] [ drop f ] if
    ] loop drop ;
