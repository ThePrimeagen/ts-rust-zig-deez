
#include <iostream>
#include <utility>
#include <gtest/gtest.h>
#include "lexer.hh"

TEST(TestLexer, TestNextToken1) {

	const TokenType testTokenTypes[] {
		TokenType::Assign,
		TokenType::Plus,
		TokenType::Lparen,
		TokenType::Rparen,
		TokenType::Lsquirly,
		TokenType::Rsquirly,
		TokenType::Comma,
		TokenType::Semicolon,
		TokenType::Eof,
	};

	Lexer lexer {"=+(){},;"};

	for (const auto testTokenType : testTokenTypes) {
		const auto token = lexer.peek();
		ASSERT_EQ(token.type, testTokenType);
		lexer.next();
	}
	ASSERT_TRUE(lexer.eof());
}

TEST(TestLexer, TestNextToken2) {

	const Token testTokens[] {
		{ TokenType::Let                  },
		{ TokenType::Identifier, "five"   },
		{ TokenType::Assign               },
		{ TokenType::Integer   , "5"      },
		{ TokenType::Semicolon            },
		{ TokenType::Let                  },
		{ TokenType::Identifier, "ten"    },
		{ TokenType::Assign               },
		{ TokenType::Integer   , "10"     },
		{ TokenType::Semicolon            },
		{ TokenType::Let                  },
		{ TokenType::Identifier, "add"    },
		{ TokenType::Assign               },
		{ TokenType::Function             },
		{ TokenType::Lparen               },
		{ TokenType::Identifier, "x"      },
		{ TokenType::Comma                },
		{ TokenType::Identifier, "y"      },
		{ TokenType::Rparen               },
		{ TokenType::Lsquirly             },
		{ TokenType::Identifier, "x"      },
		{ TokenType::Plus                 },
		{ TokenType::Identifier, "y"      },
		{ TokenType::Semicolon            },
		{ TokenType::Rsquirly             },
		{ TokenType::Semicolon            },
		{ TokenType::Let                  },
		{ TokenType::Identifier, "result" },
		{ TokenType::Assign               },
		{ TokenType::Identifier, "add"    },
		{ TokenType::Lparen               },
		{ TokenType::Identifier, "five"   },
		{ TokenType::Comma                },
		{ TokenType::Identifier, "ten"    },
		{ TokenType::Rparen               },
		{ TokenType::Semicolon            },

		{ TokenType::Bang                 },
		{ TokenType::Minus                },
		{ TokenType::Slash                },
		{ TokenType::Asterisk             },
		{ TokenType::Integer,    "5"      },
		{ TokenType::Semicolon            },
		{ TokenType::Integer,    "5"      },
		{ TokenType::Lt                   },
		{ TokenType::Integer,    "10"     },
		{ TokenType::Gt                   },
		{ TokenType::Integer,    "5"      },
		{ TokenType::Semicolon            },

		{ TokenType::Integer,    "10"     },
		{ TokenType::Eq                   },
		{ TokenType::Integer,    "10"     },
		{ TokenType::Semicolon            },
		{ TokenType::Integer,    "10"     },
		{ TokenType::Not_eq               },
		{ TokenType::Integer,    "9"      },
		{ TokenType::Semicolon            },

		{ TokenType::If                   },
		{ TokenType::Lparen               },
		{ TokenType::Integer,    "5"      },
		{ TokenType::Lt                   },
		{ TokenType::Integer,    "10"     },
		{ TokenType::Rparen               },
		{ TokenType::Lsquirly             },
		{ TokenType::Return               },
		{ TokenType::True                 },
		{ TokenType::Semicolon            },
		{ TokenType::Rsquirly             },
		{ TokenType::Else                 },
		{ TokenType::Lsquirly             },
		{ TokenType::Return               },
		{ TokenType::False                },
		{ TokenType::Semicolon            },
		{ TokenType::Rsquirly             },
						
		{ TokenType::String,     "foobar" },
		{ TokenType::String,     "foo bar"},
		{ TokenType::String,     "\r\n\"" },

		{ TokenType::Lbracket,            },
		{ TokenType::Integer,    "1"      },
		{ TokenType::Comma,               },
		{ TokenType::Integer,    "2"      },
		{ TokenType::Rbracket,            },
		{ TokenType::Semicolon,           },

		{ TokenType::Eof                  },
	};

	Lexer lexer {R"XXX(
		let five = 5;
		let ten = 10;
		let add = fn(x, y) { x + y; };

		let result = add(five, ten);

		!-/*5;
		5 < 10 > 5;

		10 == 10;
		10 != 9;

		if (5 < 10) {
			return true;
		} else {
			return false;
		}

		"foobar"
		"foo bar"
		"\r\n\""
		[1, 2];
	)XXX"};

	for (const auto& testToken : testTokens) {
		const auto token = lexer.peek();
		ASSERT_EQ((int) token.type, (int) testToken.type) << "expected " << testToken << ", got " << token;
		ASSERT_EQ(token.literal, testToken.literal);
		lexer.next();
	}
	ASSERT_TRUE(lexer.eof());
}
