
test_next_token :: () {
	input :: #string INPUT
let five = 5;
let ten = 10;
let add = fn(x, y) {
	x + y;
};

let result = add(five, ten);
! - / * 5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;	
INPUT;

	tests :: Token.[
		.{.Let			, "let"		},
		.{.Ident		, "five"	},
		.{.Assign		, "="		},
		.{.Int			, "5"		},
		.{.Semicolon	, ";"		},
		.{.Let			, "let"		},
		.{.Ident		, "ten"		},
		.{.Assign		, "="		},
		.{.Int			, "10"		},
		.{.Semicolon	, ";"		},
		.{.Let			, "let"		},
		.{.Ident		, "add"		},
		.{.Assign		, "="		},
		.{.Function		, "fn"		},
		.{.LParen		, "("		},
		.{.Ident		, "x"		},
		.{.Comma		, ","		},
		.{.Ident		, "y"		},
		.{.RParen		, ")"		},
		.{.LBrace		, "{"		},
		.{.Ident		, "x"		},
		.{.Plus			, "+"		},
		.{.Ident		, "y"		},
		.{.Semicolon	, ";"		},
		.{.RBrace		, "}"		},
		.{.Semicolon	, ";"		},
		.{.Let			, "let"		},
		.{.Ident		, "result"	},
		.{.Assign		, "="		},
		.{.Ident		, "add"		},
		.{.LParen		, "("		},
		.{.Ident		, "five"	},
		.{.Comma		, ","		},
		.{.Ident		, "ten"		},
		.{.RParen		, ")"		},
		.{.Semicolon	, ";"		},
		.{.Bang			, "!"		},
		.{.Minus		, "-"		},
		.{.Slash		, "/"		},
		.{.Asterisk		, "*"		},
		.{.Int			, "5"		},
		.{.Semicolon	, ";"		},
		.{.Int			, "5"		},
		.{.LT			, "<"		},
		.{.Int			, "10"		},
		.{.GT			, ">"		},
		.{.Int			, "5"		},
		.{.Semicolon	, ";"		},
		.{.If			, "if"		},
		.{.LParen		, "("		},
		.{.Int			, "5"		},
		.{.LT			, "<"		},
		.{.Int			, "10"		},
		.{.RParen		, ")"		},
		.{.LBrace		, "{"		},
		.{.Return		, "return"	},
		.{.True			, "true"	},
		.{.Semicolon	, ";"		},
		.{.RBrace		, "}"		},
		.{.Else			, "else"	},
		.{.LBrace		, "{"		},
		.{.Return		, "return"	},
		.{.False		, "false"	},
		.{.Semicolon	, ";"		},
		.{.RBrace		, "}"		},
		.{.Int			, "10"		},
		.{.Eq			, "=="		},
		.{.Int			, "10"		},
		.{.Semicolon	, ";"		},
		.{.Int			, "10"		},
		.{.Not_Eq		, "!="		},
		.{.Int			, "9"		},
		.{.Semicolon	, ";"		},
		.{.EOF			, "\0"		},
	];

	lex := new_lexer(input);
	read_char(*lex);

	for tt, i: tests {
		tok := next_token(*lex);

		if tok.type != tt.type {
			print("tests[%] - tokentype wrong. Expected: '%', got: '%'\n", i, tt.type, tok.type);
			exit(1);
		}

		if tok.literal != tt.literal {
			print("tests[%] - literal wrong. Expected: '%', got: '%'\n", i, tt.literal, tok.literal);
			exit(1);
		}
	}

	print("% test's ran successfully\n", tests.count);
}

