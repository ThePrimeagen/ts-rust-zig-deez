module token
    implicit none

    enum, bind(c)
        enumerator :: token_eof = 0, token_illegal
        enumerator :: token_ident, token_int

        ! Operators
        enumerator :: token_assign, token_plus, token_minus, token_bang, token_asterisk, token_slash
        enumerator :: token_lt, token_gt
        enumerator :: token_eq, token_not_eq

        ! Delimiters
        enumerator :: token_comma, token_semicolon
        enumerator :: token_lparen, token_rparen, token_lbrace, token_rbrace

        ! Keywords
        enumerator :: token_function, token_let, token_true, token_false, token_if, token_else, token_return
    end enum

    type :: token_t
        integer :: type
        character(:), allocatable :: literal
    end type
contains
    function ident_lookup(ident) result(tok)
        character(:), allocatable, intent(in) :: ident
        type(token_t) :: tok

        if (ident == "fn") then
            tok = token_t(token_function, ident)
        else if (ident == "let") then
            tok = token_t(token_let, ident)
        else if (ident == "true") then
            tok = token_t(token_true, ident)
        else if (ident == "false") then
            tok = token_t(token_false, ident)
        else if (ident == "if") then
            tok = token_t(token_if, ident)
        else if (ident == "else") then
            tok = token_t(token_else, ident)
        else if (ident == "return") then
            tok = token_t(token_return, ident)
        else
            tok = token_t(token_ident, ident)
        end if
    end function ident_lookup

    pure function token_name(token_type) result(name)
        integer, intent(in) :: token_type
        character(:), allocatable :: name

        select case (token_type)
            case (token_eof)
                name = "EOF"
            case (token_illegal)
                name = "ILLEGAL"
            case (token_ident)
                name = "IDENT"
            case (token_int)
                name = "INT"
            case (token_assign)
                name = "ASSIGN"
            case (token_plus)
                name = "PLUS"
            case (token_minus)
                name = "MINUS"
            case (token_bang)
                name = "BANG"
            case (token_asterisk)
                name = "ASTERISK"
            case (token_slash)
                name = "SLASH"
            case (token_lt)
                name = "LT"
            case (token_gt)
                name = "GT"
            case (token_eq)
                name = "EQ"
            case (token_not_eq)
                name = "NOT_EQ"
            case (token_comma)
                name = "COMMA"
            case (token_semicolon)
                name = "SEMICOLON"
            case (token_lparen)
                name = "LPAREN"
            case (token_rparen)
                name = "RPAREN"
            case (token_lbrace)
                name = "LBRACE"
            case (token_rbrace)
                name = "RBRACE"
            case (token_function)
                name = "FUNCTION"
            case (token_let)
                name = "LET"
            case (token_true)
                name = "TRUE"
            case (token_false)
                name = "FALSE"
            case (token_if)
                name = "IF"
            case (token_else)
                name = "ELSE"
            case (token_return)
                name = "RETURN"
            case default
                name = "UNKNOWN"
        end select
    end function token_name
end module token
