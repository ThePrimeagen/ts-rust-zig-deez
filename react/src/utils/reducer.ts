import { isLetter, isNumber, readIdentifier, readNumber, skipWhitespace } from './helpers';
import { Keywords, Token, TokenType, createToken } from './token';

type State = {
  token?: Token | undefined;
  buffer: string;
};

export const reducer = ({ buffer: _buffer }: State): State => {
  const buffer = skipWhitespace(_buffer);

  switch (true) {
    case buffer.startsWith('=='): {
      return { token: createToken(TokenType.Equal, '=='), buffer: buffer.substring(2) };
    }
    case buffer.startsWith('!='): {
      return { token: createToken(TokenType.NotEqual, '!='), buffer: buffer.substring(2) };
    }
    case buffer.startsWith('>='): {
      return { token: createToken(TokenType.GreaterEqual, '>='), buffer: buffer.substring(2) };
    }
    case buffer.startsWith('>'): {
      return { token: createToken(TokenType.Greater, '>'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('<='): {
      return { token: createToken(TokenType.LessEqual, '<='), buffer: buffer.substring(2) };
    }
    case buffer.startsWith('<'): {
      return { token: createToken(TokenType.Less, '<'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('!'): {
      return { token: createToken(TokenType.Bang, '!'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('='): {
      return { token: createToken(TokenType.Assign, '='), buffer: buffer.substring(1) };
    }
    case buffer.startsWith(';'): {
      return { token: createToken(TokenType.Semicolon, ';'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith(','): {
      return { token: createToken(TokenType.Comma, ','), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('('): {
      return { token: createToken(TokenType.LParen, '('), buffer: buffer.substring(1) };
    }
    case buffer.startsWith(')'): {
      return { token: createToken(TokenType.RParen, ')'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('{'): {
      return { token: createToken(TokenType.LSquirly, '{'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('}'): {
      return { token: createToken(TokenType.RSquirly, '}'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('+'): {
      return { token: createToken(TokenType.Plus, '+'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('-'): {
      return { token: createToken(TokenType.Minus, '-'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('*'): {
      return { token: createToken(TokenType.Asterisk, '*'), buffer: buffer.substring(1) };
    }
    case buffer.startsWith('/'): {
      return { token: createToken(TokenType.ForwardSlash, '/'), buffer: buffer.substring(1) };
    }
    case isLetter(buffer[0]): {
      const ident = readIdentifier(buffer);
      const keyword = Keywords[ident as keyof typeof Keywords];
      const token = keyword ?? createToken(TokenType.Ident, ident);

      return { token, buffer: buffer.substring(ident.length) };
    }
    case isNumber(buffer[0]): {
      const num = readNumber(buffer);
      return { token: createToken(TokenType.Int, num), buffer: buffer.substring(num.length) };
    }
    case buffer.startsWith('\0'): {
      return { token: createToken(TokenType.Eof, 'eof'), buffer: buffer.substring(1) };
    }
    default: {
      return { token: createToken(TokenType.Illegal, buffer[0]), buffer: buffer.substring(1) };
    }
  }
};
