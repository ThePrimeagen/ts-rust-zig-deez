module token
    implicit none

    enum, bind(c)
        enumerator :: token_eof = 0
        enumerator :: token_illegal = 1

        ! Identifiers + literals
        enumerator :: token_ident = 2
        enumerator :: token_int = 3

        ! Operators
        enumerator :: token_assign = 4
        enumerator :: token_plus = 5
        enumerator :: token_minus = 6
        enumerator :: token_bang = 7
        enumerator :: token_asterisk = 8
        enumerator :: token_slash = 9

        enumerator :: token_lt = 10
        enumerator :: token_gt = 11

        enumerator :: token_eq = 12
        enumerator :: token_not_eq = 13

        ! Delimiters
        enumerator :: token_comma = 14
        enumerator :: token_semicolon = 15

        enumerator :: token_lparen = 16
        enumerator :: token_rparen = 17
        enumerator :: token_lbrace = 18
        enumerator :: token_rbrace = 19

        ! Keywords
        enumerator :: token_function = 20
        enumerator :: token_let = 21
        enumerator :: token_true = 22
        enumerator :: token_false = 23
        enumerator :: token_if = 24
        enumerator :: token_else = 25
        enumerator :: token_return = 26
    end enum

    type :: token_t
        integer :: type
        character(len=256) :: literal
    end type

    type(token_t), dimension(7) :: keywords = [ &
        token_t(token_function, "fn"), &
        token_t(token_let, "let"), &
        token_t(token_true, "true"), &
        token_t(token_false, "false"), &
        token_t(token_if, "if"), &
        token_t(token_else, "else"), &
        token_t(token_return, "return") &
    ]
contains
    pure function ident_lookup(ident) result(tok)
        character(:), allocatable, intent(in) :: ident
        type(token_t) :: tok
        integer :: i

        tok = token_t(token_ident, ident)

        do i = 1, size(keywords)
            if (keywords(i)%literal .eq. ident) then
                tok = keywords(i)
                exit
            end if
        end do
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
