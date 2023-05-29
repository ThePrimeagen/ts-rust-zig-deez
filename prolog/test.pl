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

test(fullest) :-
        tokenize(`
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };

        let result = add(five, ten); 
        !-/ *5;
        5 < 10 > 5;

        if (5 < 10) {
            return true;
        } else {
            return false;
        }

        10 == 100;
        10 != 9; `,
            [let, ident(five), equal, int(5), semicolon, let, ident(ten), equal, int(10), semicolon, let, ident(add), equal, function, lparen, ident(x), comma, ident(y), rparen, lsquirly, ident(x), plus, ident(y), semicolon, rsquirly, semicolon, let, ident(result), equal, ident(add), lparen, ident(five), comma, ident(ten), rparen, semicolon, bang, dash, fslash, asterisk, int(5), semicolon, int(5), lt, int(10), gt, int(5), semicolon, if, lparen, int(5), lt, int(10), rparen, lsquirly, return, true, semicolon, rsquirly, else, lsquirly, return, false, semicolon, rsquirly, int(10), eq, int(100), semicolon, int(10), neq, int(9), semicolon, eof]).


:- end_tests(lists).
