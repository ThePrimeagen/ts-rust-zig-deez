unit LexerTestCases;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, LexerUnit;

type

  TLexerTestCases= class(TTestCase)
  published
    procedure TestBasicTokens;
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


initialization

  RegisterTest(TLexerTestCases);
end.

