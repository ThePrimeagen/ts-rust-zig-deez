module TokenType
    Illegal = :ILLEGAL
    Eof = :EOF
    Ident = :IDENT
    Int = :INT
    Equal = "="
    Plus = "+"
    Comma = ","
    Semicolon = ";"
    LParen = "("
    RParen = ")"
    LSquirly = "{"
    RSquirly = "}"
    Function = :FUNCTION
    Let = :LET
    True = :TRUE
    False = :FALSE
    If = :IF
    Else = :ELSE
    Return = :RETURN
    Minus = "-"
    Bang = "!"
    Asterisk = "*"
    Slash = "/"
    LT = "<"
    GT = ">"
    EQ = "=="
    NotEQ = "!="
end

def create_token(type, literal)
    { type: type, literal: literal }
end

U_0 = "0".ord
U_9 = "9".ord
U_A = "a".ord
U_Z = "z".ord
L_A = "A".ord
L_Z = "Z".ord

def is_letter(ch)
    ch = ch.ord
    (U_A <= ch && ch <= U_Z) || (L_A <= ch && ch <= L_Z) || ch == "_".ord
end

def is_digit(ch)
    ch = ch.ord
    U_0 <= ch && ch <= U_9
end

Keywords = {
    "fn" => create_token(TokenType::Function, "fn"),
    "let" => create_token(TokenType::Let, "let"),
    "true" => create_token(TokenType::True, "true"),
    "false" => create_token(TokenType::False, "false"),
    "if" => create_token(TokenType::If, "if"),
    "else" => create_token(TokenType::Else, "else"),
    "return" => create_token(TokenType::Return, "return")
}

class Tokenizer
    attr_accessor :position, :read_position, :ch
    def initialize(input)
        @input = input
        @position = 0
        @read_position = 0
        @ch = nil
        read_char
    end

    def next_token
        skip_whitespace

        tok = nil
        case @ch
        when "{"
            tok = create_token(TokenType::LSquirly, @ch)
        when "}"
            tok = create_token(TokenType::RSquirly, @ch)
        when "("
            tok = create_token(TokenType::LParen, @ch)
        when ")"
            tok = create_token(TokenType::RParen, @ch)
        when ","
            tok = create_token(TokenType::Comma, @ch)
        when ";"
            tok = create_token(TokenType::Semicolon, @ch)
        when "+"
            tok = create_token(TokenType::Plus, @ch)
        when "="
            if peek_char == "="
                ch = @ch
                read_char
                tok = create_token(TokenType::EQ, "#{ch}#{ch}")
            else
                tok = create_token(TokenType::Equal, @ch)
            end
        when "-"
            tok = create_token(TokenType::Minus, @ch)
        when "/"
            tok = create_token(TokenType::Slash, @ch)
        when "!"
            if peek_char == "="
                ch = @ch
                peeked_char = peek_char
                read_char
                tok = create_token(TokenType::NotEQ, "#{ch}#{peeked_char}")
            else
                tok = create_token(TokenType::Bang, @ch)
            end
        when "*"
            tok = create_token(TokenType::Asterisk, @ch)
        when "<"
            tok = create_token(TokenType::LT, @ch)
        when ">"
            tok = create_token(TokenType::GT, @ch)
        when "\u0000" || "\0"
            tok = create_token(TokenType::Eof, "")
        end

        if is_letter(@ch)
            literal = read_ident
            keyword = Keywords[literal]
            if keyword
                return keyword
            else
                return create_token(TokenType::Ident, literal)
            end
        elsif is_digit(@ch)
            return create_token(TokenType::Int, read_int)
        elsif !tok
            tok = create_token(TokenType::Illegal, @ch)
        end
        
        read_char
        tok
    end

    private

    def skip_whitespace
        while @ch == " " || @ch == "\t" || @ch == "\n" || @ch == "\r"
            read_char
        end
    end

    def read_char
        if @read_position >= @input.length
            @ch = "\0"
        else
            @ch = @input[@read_position]
        end
        @position = @read_position
        @read_position += 1
    end

    def peek_char
        if @read_position >= @input.length
            return "\0"
        else
            return @input[@read_position]
        end
    end

    def read_ident
        position = @position
      
        while is_letter(@ch)
          read_char
        end
      
        @input[position...@position]
    end

    def read_int
        position = @position
      
        while is_digit(@ch)
          read_char
        end
      
        @input[position...@position]
    end  
end
  