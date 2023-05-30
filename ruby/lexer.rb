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
    "let" => create_token(TokenType::Let, "let")
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
            tok = create_token(TokenType::Equal, @ch)
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
  