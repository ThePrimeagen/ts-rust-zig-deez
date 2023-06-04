program LexerProgram;
uses SysUtils, LexerUnit;

var
    lex: Lexer;
    Input: String;
    c: Char;
begin
    Input := '=+(){},;';
    lex := Lexer.Create(Input);
    for c in Input do
    begin
        with lex.GetNextToken() do
            WriteLn('Type: ', TokenTypeToString(Token_Type), ', Literal: ', Literal);
    end;
    with lex.GetNextToken() do
        WriteLn('Type: ', TokenTypeToString(Token_Type), ', Literal: ', Literal);
end.

