package src;

import src.Token;

using StringTools;
using src.CharTools;

class Lexer {
	var currentPos:Int = 0;
	var currentCharCode:Int;
	final input:String;

	public function new(input:String) {
		this.input = input;
		currentCharCode = input.fastCodeAt(currentPos);
	}

	private function movePos() {
		if (currentPos >= input.length) {
			currentPos = -1;
			return;
		}
		currentPos++;
		currentCharCode = input.fastCodeAt(currentPos);
	}

	private function skipWhitespace() {
		var loop = true;
		while (loop) {
			switch (currentCharCode) {
				case " ".code | "\t".code | "\n".code | "\r".code:
					movePos();
				default:
					loop = false;
			}
		}
	}

	public function nextToken():Token {
		skipWhitespace();

		if (StringTools.isEof(currentCharCode)) {
			return {
				type: TokenType.Eof,
				literal: "eof"
			}
		}

		if (currentCharCode.isNumber()) {
			var startPos = currentPos;

			while (currentCharCode.isNumber()) {
				movePos();
			}

			return {
				type: TokenType.Int,
				literal: input.substring(startPos, currentPos)
			}
		}

		if (currentCharCode.isLetter()) {
			var startPos = currentPos;

			while (currentCharCode.isLetter()) {
				movePos();
			}

			var ident = input.substring(startPos, currentPos);
			var type = TokenType.Ident;

			switch (ident) {
				case "fn":
					type = TokenType.Function;
				case "let":
					type = TokenType.Let;
				case "return":
					type = TokenType.Return;
				case "true":
					type = TokenType.True;
				case "false":
					type = TokenType.False;
				case "if":
					type = TokenType.If;
				case "else":
					type = TokenType.Else;
			}

			return {
				type: type,
				literal: ident
			}
		}

		var type = TokenType.Illegal;
		switch (currentCharCode) {
			case '{'.code:
				type = TokenType.LSquirly;
			case '}'.code:
				type = TokenType.RSquirly;
			case '('.code:
				type = TokenType.LParen;
			case ')'.code:
				type = TokenType.RParen;
			case ','.code:
				type = TokenType.Comma;
			case "!".code:
				var nextCode = this.input.fastCodeAt(currentPos + 1);
				if (nextCode == "=".code) {
					this.movePos();
					this.movePos();
					return {
						type: TokenType.NotEqual,
						literal: "!="
					};
				} else {
					type = TokenType.Bang;
				}
			case ">".code:
				type = TokenType.GreaterThan;
			case "<".code:
				type = TokenType.LessThan;
			case "*".code:
				type = TokenType.Asterisk;
			case "/".code:
				type = TokenType.ForwardSlash;
			case "-".code:
				type = TokenType.Dash;
			case ";".code:
				type = TokenType.Semicolon;
			case "+".code:
				type = TokenType.Plus;
			case "=".code:
				var nextCharCode = this.input.fastCodeAt(currentPos + 1);
				if (nextCharCode == "=".code) {
					this.movePos();
					this.movePos();
					return {type: TokenType.Equal, literal: "=="};
				} else {
					type = TokenType.Assign;
				}
		}

		var token = {
			type: type,
			literal: String.fromCharCode(currentCharCode)
		};

		movePos();

		return token;
	}
}
