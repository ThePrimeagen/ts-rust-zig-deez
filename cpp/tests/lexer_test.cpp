
#include <iostream>
#include <utility>
#include <gtest/gtest.h>
#include "lexer/lexer.hh"

TEST(TestLexer, TestLexer_One) {

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
}

TEST(TestLexer, TestLexer_Complete) {

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
		{ TokenType::Eof                  },
	};

	Lexer lexer {
	  "let five = 5;"
	  "let ten = 10;"
	  "let add = fn(x, y) { x + y; };"
	  "let result = add(five, ten);"
	};

	for (const auto& testToken : testTokens) {
		const auto token = lexer.peek();
		ASSERT_EQ((int) token.type, (int) testToken.type);
		ASSERT_EQ(token.literal, testToken.literal);
		lexer.next();
	}
}
