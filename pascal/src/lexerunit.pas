unit LexerUnit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
    TokenType = (Illegal, Eof, Ident, Int, Equal, Plus, Comma, Semicolon, LParen, RParen, LSquirly, RSquirly, FunctionType, Let);

    Token = record
        Token_Type: TokenType;
        Literal: String;
    end;

    TKeywords = specialize TFPGMap<String, TokenType>;

    Lexer = class
        Input: String;
        InputLength: Integer;
        Position: Integer;
        ReadPosition: Integer;
        Ch: Char;
        Keywords: TKeywords;

        constructor Create(InputCode: String);
        procedure ReadChar();
        function CreateToken(Token_Type: TokenType; Literal: string): Token;
        function LookupIdent(ident: string): TokenType;
        function GetNextToken(): Token;
        function ReadInt(): String;
        function ReadIdent(): String;
        procedure SkipWhitespace();
    end;

    function TokenTypeToString(tokenType: TokenType): String;

implementation

constructor Lexer.Create(InputCode: String);
begin
    Keywords:= TKeywords.Create();
    Keywords.Add('fn', TokenType.FunctionType);
    Keywords.Add('let', TokenType.Let);

    self.Input := InputCode;
    self.InputLength := Length(InputCode);
    self.Position := 0;
    self.ReadPosition := 0;
    self.Ch := #0;
    self.ReadChar();
end;

procedure Lexer.ReadChar();
begin
    if self.ReadPosition >= self.InputLength then
        self.Ch := #0
    else
        self.Ch := self.Input[self.ReadPosition + 1];
    self.Position := self.ReadPosition;
    Inc(self.ReadPosition);
end;

function Lexer.CreateToken(Token_Type: TokenType; Literal: string): Token;
var
    tok: Token;
begin
    tok.Token_Type:= Token_Type;
    tok.Literal:= Literal;

    Result:= tok;
end;

function IsAlpha(c: Char): Boolean;
begin
    Result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z')) or (c = '_');
end;

function IsDigit(c: Char): Boolean;
begin
    Result := (c >= '0') and (c <= '9');
end;

function IsSpace(c: Char): Boolean;
begin
    Result := (c = ' ') or (c = #9) or (c = #10) or (c = #13);
end;

function Lexer.LookupIdent(ident: string): TokenType;
var
    index: integer;
begin
     if Keywords.IndexOf(ident) >= 0 then
     begin
         Result:= Keywords[ident];
     end
     else
         Result:= TokenType.Ident;
end;

function Lexer.GetNextToken(): Token;
var
    tok: Token;
    indent: String;
begin
    self.SkipWhitespace();

    if Ch = '=' then tok := CreateToken(Equal, '=') else
    if Ch = '+' then tok := CreateToken(Plus, '+') else
    if Ch = ',' then tok := CreateToken(Comma, ',') else
    if Ch = ';' then tok := CreateToken(Semicolon, ';') else
    if Ch = '(' then tok := CreateToken(LParen, '(') else
    if Ch = ')' then tok := CreateToken(RParen, ')') else
    if Ch = '{' then tok := CreateToken(LSquirly, '{') else
    if Ch = '}' then tok := CreateToken(RSquirly, '}') else
    if Ch = #0 then tok := CreateToken(Eof, '') else
    if (IsAlpha(Ch)) then
    begin
        indent := self.ReadIdent();

        tok := CreateToken(LookupIdent(indent), indent);
        Result:= tok;
        exit;
    end else
    if IsDigit(Ch) then
    begin
        tok := CreateToken(Int, self.ReadInt());
        Result:= tok;
        exit;
    end
    else
        tok := CreateToken(Illegal, Ch);

    self.ReadChar();
    Result := tok;
end;

function Lexer.ReadInt(): String;
var
    pos: Integer;
begin
    pos := self.Position;
    while (self.Ch >= '0') and (self.Ch <= '9') do
        self.ReadChar();
    Result := Copy(self.Input, pos + 1, self.Position - pos);
end;

function Lexer.ReadIdent(): String;
var
    pos: Integer;
begin
    pos := self.Position;
    while IsAlpha(self.Ch) or (self.Ch = '_') do
        self.ReadChar();
    Result := Copy(self.Input, pos + 1, self.Position - pos);
end;

procedure Lexer.SkipWhitespace();
begin
    while IsSpace(self.Ch) do
        self.ReadChar();
end;

function TokenTypeToString(tokenType: TokenType): String;
begin
    case tokenType of
        Illegal: Result := 'ILLEGAL';
        Eof: Result := 'EOF';
        Ident: Result := 'IDENT';
        Int: Result := 'INT';
        Equal: Result := '=';
        Plus: Result := '+';
        Comma: Result := ',';
        Semicolon: Result := ';';
        LParen: Result := '(';
        RParen: Result := ')';
        LSquirly: Result := '{';
        RSquirly: Result := '}';
        FunctionType: Result := 'FUNCTION';
        Let: Result := 'LET';
    else
        Result := '';
    end;
end;


end.

