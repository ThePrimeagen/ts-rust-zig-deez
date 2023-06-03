module lexer
    use token
    implicit none

    type :: lexer_t
        character(:), allocatable :: input
        integer :: position
        integer :: read_position
        character :: ch
    end type

contains
    pure function is_whitepsace(ch) result(ret)
        character, intent(in) :: ch
        logical :: ret
        integer :: ord

        ord = ichar(ch)
        if (ord == 32 .or. ord == 9 .or. ord == 13 .or. ord == 10) then
            ret = .true.
        else
            ret = .false.
        end if
    end function is_whitepsace

    subroutine lexer_init(l, input)
        character(:), allocatable, intent(in) :: input
        type(lexer_t), intent(out) :: l

        l = lexer_t(input, 1, 1, 'x')
        call lexer_read_ch(l)
    end subroutine lexer_init

    subroutine lexer_next_token(l, tok)
        type(lexer_t), intent(inout) :: l
        type(token_t), intent(out) :: tok
        character :: ch
        character(:), allocatable :: lit

        call lexer_skip_ws(l)

        token_case: select case(l%ch)
            case ('=')
                if (lexer_peek_char(l) == '=') then
                    ch = l%ch
                    call lexer_read_ch(l)
                    tok = token_t(token_eq, ch // l%ch)
                else
                    tok = token_t(token_assign, l%ch)
                end if

            case ('+')
                tok = token_t(token_plus, "+")

            case ('-')
                tok = token_t(token_minus, "-")

            case ('!')
                if (lexer_peek_char(l) == '=') then
                    ch = l%ch
                    call lexer_read_ch(l)
                    tok = token_t(token_not_eq, ch // l%ch)
                else
                    tok = token_t(token_bang, l%ch)
                end if

            case ('/')
                tok = token_t(token_slash, l%ch)

            case ('*')
                tok = token_t(token_asterisk, l%ch)

            case ('<')
                tok = token_t(token_lt, l%ch)

            case ('>')
                tok = token_t(token_gt, l%ch)

            case (';')
                tok = token_t(token_semicolon, l%ch)

            case (',')
                tok = token_t(token_comma, l%ch)

            case ('{')
                tok = token_t(token_lbrace, l%ch)

            case ('}')
                tok = token_t(token_rbrace, l%ch)

            case ('(')
                tok = token_t(token_lparen, l%ch)

            case (')')
                tok = token_t(token_rparen, l%ch)

            case (achar(0))
                tok = token_t(token_eof, "")

            case default
                if (is_letter(l%ch)) then
                    call lexer_read_ident(l, lit)
                    tok = ident_lookup(lit)
                    return
                else if (is_digit(l%ch)) then
                    call lexer_read_number(l, lit)
                    tok = token_t(token_int, lit)
                    return
                else
                    tok = token_t(token_illegal, l%ch)
                end if
        end select token_case

        call lexer_read_ch(l)
    end subroutine

    subroutine lexer_skip_ws(l)
        type(lexer_t), intent(inout) :: l

        do while (is_whitepsace(l%ch))
            call lexer_read_ch(l)
        end do
    end subroutine lexer_skip_ws

    subroutine lexer_read_ch(l)
        type(lexer_t), intent(inout) :: l

        if (l%read_position > len(l%input)) then
            l%ch = achar(0)
        else
            l%ch = l%input(l%read_position:l%read_position)
        end if
        l%position = l%read_position
        l%read_position = l%read_position + 1
    end subroutine lexer_read_ch

    subroutine lexer_read_ident(l, ident)
        type(lexer_t), intent(inout) :: l
        character(:), allocatable, intent(out) :: ident
        integer :: position

        position = l%position
        do while (is_letter(l%ch))
            call lexer_read_ch(l)
        end do

        ident = l%input(position:l%position-1)
    end subroutine lexer_read_ident

    subroutine lexer_read_number(l, number)
        type(lexer_t), intent(inout) :: l
        character(:), allocatable, intent(out) :: number
        integer :: position

        position = l%position
        do while (is_digit(l%ch))
            call lexer_read_ch(l)
        end do

        number = l%input(position:l%position-1)
    end subroutine lexer_read_number

    function lexer_peek_char(l) result(ch)
        type(lexer_t) :: l
        character :: ch

        if (l%read_position > len(l%input)) then
            ch = achar(0)
        else
            ch = l%input(l%read_position:l%read_position)
        end if
    end function lexer_peek_char

    pure function is_letter(ch) result(ret)
        character, intent(in) :: ch
        logical :: ret

        if ((ch >= 'a' .and. ch <= 'z') .or. (ch >= 'A' .and. ch <= 'Z') .or. ch == '_') then
            ret = .true.
        else
            ret = .false.
        end if
    end function is_letter

    pure function is_digit(ch) result(ret)
        character, intent(in) :: ch
        logical :: ret

        if (ch >= '0' .and. ch <= '9') then
            ret = .true.
        else
            ret = .false.
        end if
    end function is_digit
end module lexer
