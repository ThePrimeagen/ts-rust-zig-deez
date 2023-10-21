import { render, screen } from '@testing-library/react';

import Lexer from 'lexer/Lexer';

test('test#1', () => {
  const input = `
    let five = 5;
  `;
  render(<Lexer input={input} />);

  const expectedOutput = `(LET: let)(IDENT: five)(ASSIGN: =)(INT: 5)(SEMICOLON: ;)`;
  expect(screen.getByText(expectedOutput)).toBeInTheDocument();
});

test('test#2', () => {
  const input = `
    let five % 5;
  `;
  render(<Lexer input={input} />);

  const expectedOutput = `(LET: let)(IDENT: five)(ILLEGAL: %)(INT: 5)(SEMICOLON: ;)`;
  expect(screen.getByText(expectedOutput)).toBeInTheDocument();
});

test('test#3', () => {
  const input = `
    let five = 5;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;
    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
  `;
  render(<Lexer input={input} />);

  const expectedOutput = `(LET: let)(IDENT: five)(ASSIGN: =)(INT: 5)(SEMICOLON: ;)(LET: let)(IDENT: add)(ASSIGN: =)(FUNCTION: fn)(L_PAREN: ()(IDENT: x)(COMMA: ,)(IDENT: y)(R_PARENT: ))(L_SQUIRLY: {)(IDENT: x)(PLUS: +)(IDENT: y)(SEMICOLON: ;)(R_SQUIRLY: })(SEMICOLON: ;)(LET: let)(IDENT: result)(ASSIGN: =)(IDENT: add)(L_PAREN: ()(IDENT: five)(COMMA: ,)(IDENT: ten)(R_PARENT: ))(SEMICOLON: ;)(BANG: !)(MINUS: -)(FORWARD_SLASH: /)(ASTERISK: *)(INT: 5)(SEMICOLON: ;)(INT: 5)(LESS: <)(INT: 10)(GREATER: >)(INT: 5)(SEMICOLON: ;)(IF: if)(L_PAREN: ()(INT: 5)(LESS: <)(INT: 10)(R_PARENT: ))(L_SQUIRLY: {)(RETURN: return)(TRUE: true)(SEMICOLON: ;)(R_SQUIRLY: })(ELSE: else)(L_SQUIRLY: {)(RETURN: return)(FALSE: false)(SEMICOLON: ;)(R_SQUIRLY: })(INT: 10)(EQUAL: ==)(INT: 10)(SEMICOLON: ;)(INT: 10)(NOT_EQUAL: !=)(INT: 9)(SEMICOLON: ;)`;
  expect(screen.getByText(expectedOutput)).toBeInTheDocument();
});
