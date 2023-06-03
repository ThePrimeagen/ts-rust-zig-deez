USING: kernel prettyprint arrays strings math accessors 
    sequences combinators namespaces vectors assocs 
    combinators.short-circuit sbufs ;
IN: deez.lexer

SYMBOLS: tkIllegal tkEOF tkIdent tkInt
    tkAssign tkEq tkBang tkNEq 
    tkPlus tkMinus tkStar tkSlash 
    tkLT tkGT tkLTEq tkGTEq 
    tkLParen tkRParen tkLSquirly tkRSquirly tkComma 
    tkFunction tkLet tkReturn
    tkTrue tkFalse tkIf tkElse ;

CONSTANT: token-lookup-table H{
    { "="       tkAssign    }   { "=="      tkEq        }
    { "!"       tkBang      }   { "!="      tkNEq       }
    { "+"       tkPlus      }   { "-"       tkMinus     }
    { "*"       tkStar      }   { "/"       tkSlash     }
    { "<"       tkLT        }   { ">"       tkGT        }
    { "<="      tkLTEq      }   { ">="      tkGTEq      }
    { "("       tkLParen    }   { ")"       tkRParen    }
    { "{"       tkLSquirly  }   { "}"       tkRSquirly  }
    { ","       tkComma     }
    { "fn"      tkFunction  }
    { "let"     tkLet       }
    { "true"    tkTrue      }
    { "false"   tkFalse     }
    { "if"      tkIf        }
    { "else"    tkElse      }
    { "return"  tkReturn    }
}

: token-lookup ( ch -- sym ) 1string token-lookup-table at tkIllegal or ;

: ?token-lookup ( ch/f -- sym ) [ token-lookup ] [ tkIllegal ] if* ;

: letter? ( c -- t/f )
    { [ CHAR: a >= ] [ CHAR: z <= and ] 
      [ CHAR: A >= ] [ CHAR: Z <= and ] 
      [ CHAR: _ = or or ] } cleave ;

: whitespace? ( c -- t/f ) 
    { [ CHAR: space = ] [ CHAR: \t = ] [ CHAR: \n = ] [ CHAR: \r = ] } 1|| ;

: digit? ( c -- t/f ) { [ CHAR: 0 >= ] [ CHAR: 9 <= ] } 1&& ;

: keyword-or-ident ( string -- kw/ident ) 
    token-lookup-table at [ tkIdent ] unless* ;

TUPLE: tokentype literal type ;
C: <tokentype> tokentype

TUPLE: lexer
 { text string }
 { cursor fixnum }
 { line fixnum }
 { col fixnum } ;

: <lexer> ( text -- lexer ) 0 1 1 lexer boa ;

: lexing? ( lexer -- lexer t/f ) dup [ cursor>> ] [ text>> length ] bi < ;

: inc-cursor ( lexer -- lexer ) [ 1 + ] change-cursor ;

: dec-cursor ( lexer -- lexer ) [ 1 - ] change-cursor ;

: current-char? ( lexer -- lexer ch/f )
    lexing? [ dup [ cursor>> ] [ text>> ] bi nth ] [ f ] if ;

: next-char ( lexer -- lexer ch )
    dup [ cursor>> ] [ text>> ] bi nth [ inc-cursor ] dip ;

: next-char? ( lexer -- lexer ch/f ) lexing? [ next-char ] [ f ] if ;

: read-identifier ( lexer ch -- lexer string ) 
    1string >sbuf swap 
    [ next-char? dup [ dup letter? [ drop dec-cursor f ] unless ] when ] 
    [ pick push ] while* swap "" like ;

: read-number ( lexer ch -- lexer string )
    1string >sbuf swap
    [ next-char? dup [ dup digit? [ drop dec-cursor f ] unless ] when ] 
    [ pick push ] while* swap "" like ;

: skip-whitespace ( lexer -- lexer )
    lexing? [ [ current-char? whitespace? ] [ inc-cursor ] while ] when ;

: next-token? ( lexer -- lexer token/f )
    skip-whitespace next-char? [ {
        { [ dup letter? ] [ read-identifier dup keyword-or-ident ] }
        { [ dup digit?  ] [ read-number tkInt ] }
        [ dup token-lookup {
            { tkAssign [ [ current-char? ] dip swap ?token-lookup tkAssign
                = [ [ inc-cursor ] dip tkEq ] [ tkAssign ] if ] }
            { tkBang [ [ current-char? ] dip swap ?token-lookup tkAssign
                = [ [ inc-cursor ] dip tkNEq ] [ tkBang ] if ] }
            { tkLT [ [ current-char? ] dip swap ?token-lookup tkAssign
                = [ [ inc-cursor ] dip tkLTEq ] [ tkLT ] if ] }
            { tkGT [ [ current-char? ] dip swap ?token-lookup tkAssign
                = [ [ inc-cursor ] dip tkGTEq ] [ tkGT ] if ] }
            [  ] ! case: default does not pop the matched item
        } case ]
    } cond <tokentype> ] [ f ] if* ;

: lex ( lexer -- seq )
    0 <vector> swap [ 
        next-token? [ pick push t ] [ f ] if*
    ] loop drop ;

