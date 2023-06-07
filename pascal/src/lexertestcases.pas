unit LexerTestCases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LexerUnit;

type

  TLexerTestCases= class(TTestCase)
  published
    procedure TestBasicTokens;
    procedure TestAddFunctionMonkeyCode;
  end;

implementation

procedure TLexerTestCases.TestBasicTokens;
var
    lex: Lexer;
    Input: String;
    c: Char;
    index: integer;
begin
    Input := '=+(){},;';
    lex := Lexer.Create(Input);

    index:= 1;
    for c in Input do
    begin
        with lex.GetNextToken() do
            begin
                if Input[index] <> Literal then
                   begin
                        Fail('Token from input "' + Input[index] + '" does not match literal: "' + Literal + '"');
                   end;
            end;
        index:= index + 1;
    end;
end;

procedure TLexerTestCases.TestAddFunctionMonkeyCode;
var
    lex: Lexer;
    code: String;
    expectedTokens: array of Token = (
     (Token_Type: TokenType.Let; Literal: 'let'),
     (Token_Type: TokenType.Ident; Literal: 'five'),
     (Token_Type: TokenType.Equal; Literal: '='),
     (Token_Type: TokenType.Int; Literal: '5'),
     (Token_Type: TokenType.Semicolon; Literal: ';'),
     (Token_Type: TokenType.Let; Literal: 'let'),
     (Token_Type: TokenType.Ident; Literal: 'ten'),
     (Token_Type: TokenType.Equal; Literal: '='),
     (Token_Type: TokenType.Int; Literal: '10'),
     (Token_Type: TokenType.Semicolon; Literal: ';'),
     (Token_Type: TokenType.Let; Literal: 'let'),
     (Token_Type: TokenType.Ident; Literal: 'add'),
     (Token_Type: TokenType.Equal; Literal: '='),
     (Token_Type: TokenType.FunctionType; Literal: 'fn'),
     (Token_Type: TokenType.LParen; Literal: '('),
     (Token_Type: TokenType.Ident; Literal: 'x'),
     (Token_Type: TokenType.Comma; Literal: ','),
     (Token_Type: TokenType.Ident; Literal: 'y'),
     (Token_Type: TokenType.RParen; Literal: ')'),
     (Token_Type: TokenType.LSquirly; Literal: '{'),
     (Token_Type: TokenType.Ident; Literal: 'x'),
     (Token_Type: TokenType.Plus; Literal: '+'),
     (Token_Type: TokenType.Ident; Literal: 'y'),
     (Token_Type: TokenType.Semicolon; Literal: ';'),
     (Token_Type: TokenType.RSquirly; Literal: '}'),
     (Token_Type: TokenType.Semicolon; Literal: ';'),
     (Token_Type: TokenType.Let; Literal: 'let'),
     (Token_Type: TokenType.Ident; Literal: 'result'),
     (Token_Type: TokenType.Equal; Literal: '='),
     (Token_Type: TokenType.Ident; Literal: 'add'),
     (Token_Type: TokenType.LParen; Literal: '('),
     (Token_Type: TokenType.Ident; Literal: 'five'),
     (Token_Type: TokenType.Comma; Literal: ','),
     (Token_Type: TokenType.Ident; Literal: 'ten'),
     (Token_Type: TokenType.RParen; Literal: ')'),
     (Token_Type: TokenType.Semicolon; Literal: ';'),
     (Token_Type: TokenType.Eof; Literal: '')
     );
     index: integer;
     currentToken: Token;
     ctokenType: TokenType;
     cliteral: string;
begin
     code:= 'let five = 5;' + LineEnding +
            'let ten = 10;' + LineEnding +
            'let add = fn(x, y) {' + LineEnding +
            '    x + y;' + LineEnding +
            '};' + LineEnding +
            'let result = add(five, ten);' + LineEnding;

     lex := Lexer.Create(code);

     for index:= 0 to length(expectedTokens) - 1 do
     begin
          with lex.GetNextToken() do
          begin
              ctokenType:= Token_Type;
              cliteral:= Literal;
              currentToken:= expectedTokens[index];
              if (Token_Type <> currentToken.Token_Type) or
                 (Literal <> currentToken.Literal) then
                 begin
                     Fail('Expected at index: ' + IntToStr(index) + ' -> ' + TokenTypeToString(currentToken.Token_Type) + '-' + currentToken.Literal + ' but got ' + TokenTypeToString(Token_Type) + '-' + Literal);
                 end;
          end;
     end;

end;

initialization

  RegisterTest(TLexerTestCases);
end.

