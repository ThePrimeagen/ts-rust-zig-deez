from dataclasses import (
    dataclass,
)

import deez_py.token_types as TokenType

from .tokens import (
    FIXED_TOKENS,
    RESERVED_KEYWORDS,
    Token,
)


@dataclass
class Lexer:
    """
    Lexer tokenizes an input string into a sequence of tokens.

    Args:
        input (str): The input string to be tokenized.
        position (int): Current position in the input string. Default is 0.
        read_position (int): Current position for reading the next character. Default is 0.
        ch (str): The current character being processed. Default is an empty string.

    Attributes:
        input (str): The input string to be tokenized.
        input_length (int): The length of the input string.
        position (int): Current position in the input string.
        read_position (int): Current position for reading the next character.
        ch (str): The current character being processed.

    Examples:
        >>> lex = Lexer('=+(){},;')
        >>> for _ in range(9):
        >>>     print(lex.get_next_token())
        ...
        Token(type='=', literal='=')
        Token(type='+', literal='+')
        Token(type='(', literal='(')
        Token(type=')', literal=')')
        Token(type='{', literal='{')
        Token(type='}', literal='}')
        Token(type=',', literal=',')
        Token(type=';', literal=';')
        Token(type='EOF', literal='EOF')
    """

    input: str
    position: int = 0
    read_position: int = 0
    ch: str = ""

    def __post_init__(self) -> None:
        """
        Post-initialization method called after the class is initialized.

        This method initializes the input length and reads the first character from the input string.
        """
        self.input_length = len(self.input)
        self.read_char()

    def read_char(self) -> None:
        """
        Moves the lexer's position to the next character in the input string.

        This method advances the lexer's position by one and updates the current character (`self.ch`)
        to the next character in the input string. It is useful for iterating over the input characters
        manually when more control is needed.

        Examples:
            >>> lex = Lexer("abc")
            >>> lex.ch
            'a'
            >>> lex.read_char()
            >>> lex.ch
            'b'
            >>> lex.read_char()
            >>> lex.ch
            'c'
        """
        self.ch = self.peek_char()
        self.position = self.read_position
        self.read_position += 1

    def peek_char(self) -> str:
        """
        Returns the next character in the input string without advancing the lexer's position.

        If the lexer's read position (`self.read_position`) is greater than or equal to the length
        of the input string, the null character ("\0") is returned. Otherwise, the character at the
        current read position is returned.

        Returns:
            str: The next character in the input string.

        Examples:
            >>> lex = Lexer("abc")
            >>> lex.peek_char()
            'a'
            >>> lex.read_position = 1
            >>> lex.peek_char()
            'b'
            >>> lex.read_position = 2
            >>> lex.peek_char()
            'c'
            >>> lex.read_position = 3
            >>> lex.peek_char()
            '\\0'
        """
        if self.read_position >= self.input_length:
            return "\0"
        return self.input[self.read_position]

    def reset(self) -> None:
        """Resets the lexer's position to the beginning.

        This method resets the lexer's position to the beginning of the input string by setting both the
        current position (`self.position`) and the position for the next character (`self.read_position`) to 0.
        After resetting the position, the current character (`self.ch`) is updated to the first character in
        the input string.

        Example:
            >>> lexer = Lexer("abc")
            >>> lexer.read_char()
            >>> lexer.position
            1
            >>> lexer.read_position
            2
            >>> lexer.reset()
            >>> lexer.position
            0
            >>> lexer.read_position
            1
            >>> lexer.ch
            "a"
        """
        self.position = 0
        self.read_position = 0
        self.read_char()

    def skip_until(self, token_name):
        """
        Skips characters in the input string until the specified token is encountered.

        This method moves the lexer's position forward by skipping characters in the input string
        until the specified token is found. The lexer will continue to advance until it encounters
        the specified token or reaches the end of the input string.

        Args:
            token_name (TokenType): The name of the token to skip until.

        Example:
            >>> lexer = Lexer("abc = 12;3")
            >>> lexer.skip_until(TokenType.Semicolon)
            >>> lexer.position
            9
            >>> lexer.read_position
            10
            >>> lexer.ch
            '3'
        """
        while self.ch != "\x00" and self.get_next_token().type != token_name:
            ...

    def skip_tokens(self, token_names):
        """
        Skips characters in the input string until any of the specified tokens are encountered.

        This method moves the lexer's position forward by skipping characters in the input string
        until any of the specified tokens are found. The lexer will continue to advance until it
        encounters any of the specified tokens or reaches the end of the input string.

        Args:
            token_names (list[TokenType]): A list of token names to skip until.

        Example:
            >>> lexer = Lexer("let x =42;")
            >>> lexer.skip_tokens([TokenType.Equal, TokenType.Semicolon])
            >>> lexer.position
            7
            >>> lexer.read_position
            8
            >>> lexer.ch
            '4'
        """
        while (
            self.ch != "\x00" and self.get_next_token().type not in token_names
        ):
            ...

    def get_next_token(self) -> Token:
        """
        Get the next token from the input string.

        Returns:
            Token: The next token.

        Examples:
            >>> lex = Lexer('=+(){},;')
            >>> lex.get_next_token()
            Token(type=<TokenType.Equal: '='>, literal='=')
        """
        self.skip_whitespace()

        # I dont wanna write if else for each character
        tok = None
        if self.ch in FIXED_TOKENS:
            tok = FIXED_TOKENS[self.ch]
        match tok:
            case Token(TokenType.ASSING):
                # PEEK
                if self.peek_char() == "=":
                    self.read_char()
                    tok = FIXED_TOKENS["=="]
            case Token(TokenType.BANG):
                # PEEK
                if self.peek_char() == "=":
                    self.read_char()
                    tok = FIXED_TOKENS["!="]
            case None:
                if self.is_letter(self.ch):
                    ident = self.read_ident()
                    if ident in RESERVED_KEYWORDS:
                        return RESERVED_KEYWORDS[ident]
                    return Token(TokenType.IDENT, ident)
                elif self.ch.isdigit():
                    return Token(TokenType.INT, self.read_int())
                tok = Token(TokenType.ILLEGAL, TokenType.ILLEGAL)

        self.read_char()
        return tok

    def read_int(self) -> str:
        """
        Read an integer from the input string.

        Returns:
            str: The integer as a string.

        Examples:
            >>> lex = Lexer('123')
            >>> lex.read_int()
            '123
        """
        pos = self.position

        while self.ch.isdigit():
            self.read_char()

        return self.input[pos : self.position]

    def read_ident(self) -> str:
        """
        Read an identifier from the input string.

        Returns:
            str: The identifier.

        Examples:
            >>> lex = Lexer('let')
            >>> lex.read_ident()
            'let'
        """
        pos = self.position

        while self.is_letter(self.ch):
            self.read_char()

        return self.input[pos : self.position]

    def skip_whitespace(self) -> None:
        """
        Skip whitespace characters in the input string.

        This method advances the read position until a non-whitespace character is encountered.

        Examples:
            >>> lex = Lexer("  let x = 5;")
            >>> lex.skip_whitespace()
            >>> lex.ch
            'l'
        """
        while self.ch.isspace():
            self.read_char()

    @staticmethod
    def is_letter(ch: str) -> bool:
        """
        Check if a character is a letter or underscore.

        Args:
            ch (str): The character to be checked.

        Returns:
            bool: True if the character is a letter or underscore, False otherwise.

        Examples:
            >>> Lexer.is_letter('a')
            True
            >>> Lexer.is_letter('7')
            False
            >>> Lexer.is_letter('_')
            True
        """
        return ch.isalpha() or ch == "_"


__all__ = ["Lexer"]
