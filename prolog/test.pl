:- begin_tests(lists).
:- use_module(tokenizer, [tokenize/2]).

test(simple) :-
        tokenize(`=+(){},;`,
            [equal, plus, lparen, rparen, lsquirly, rsquirly, comma, semicolon, eof]).

test(full) :-
        tokenize(`
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten); `,
            [let, ident(five), equal, int(5), semicolon, let, ident(ten), equal, int(10), semicolon, let, ident(add), equal, function, lparen, ident(x), comma, ident(y), rparen, lsquirly, ident(x), plus, ident(y), semicolon, rsquirly, semicolon, let, ident(result), equal, ident(add), lparen, ident(five), comma, ident(ten), rparen, semicolon, eof]).

:- end_tests(lists).
