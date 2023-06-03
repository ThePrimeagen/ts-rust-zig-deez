program LexerProgram;
uses SysUtils;

type
    TokenType = (Illegal, Eof, Ident, Int, Equal, Plus, Comma, Semicolon, LParen, RParen, LSquirly, RSquirly, FunctionType, Let);

type
    Token = record
        Token_Type: TokenType;
        Literal: String;
    end;

type
    Lexer = class
        Input: String;
        InputLength: Integer;
        Position: Integer;
        ReadPosition: Integer;
        Ch: Char;

        constructor Create(InputCode: String);
        procedure ReadChar();
        function GetNextToken(): Token;
        function ReadInt(): String;
        function ReadIdent(): String;
        procedure SkipWhitespace();
    end;

constructor Lexer.Create(InputCode: String);
begin
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

function CreateToken(Token_Type: TokenType; Literal: string): Token;
var
    tok: Token;
begin
    tok.Token_Type:= Token_Type;
    tok.Literal:= Literal;

    Result:= tok;
end;

function IsAlpha(c: Char): Boolean;
begin
    Result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

function IsDigit(c: Char): Boolean;
begin
    Result := (c >= '0') and (c <= '9');
end;

function IsSpace(c: Char): Boolean;
begin
    Result := (c = ' ') or (c = #9) or (c = #10) or (c = #13);
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
    if Ch = #0 then tok := CreateToken(Eof, #0) else
    if (IsAlpha(Ch)) or (Ch = '_') then
    begin
        indent := self.ReadIdent();
        if indent = 'fn' then tok := CreateToken(FunctionType, 'fn') else
        if indent = 'let' then tok := CreateToken(Let, 'let') else
        tok := CreateToken(Ident, indent);
    end else
    if IsDigit(Ch) then
        tok := CreateToken(Int, self.ReadInt())
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

