module parser
    use list
    use token
    use lexer
    use ast

    implicit none

    enum, bind(c)
        enumerator :: plowest = 1, pequals, plesgreater
        enumerator :: psum, pproduct, pprefix, pcall
    end enum

    type :: token_map
        integer :: key, value
    end type

    type(token_map), dimension(9) :: precedences = [&
        token_map(token_eq, pequals), &
        token_map(token_not_eq, pequals), &
        token_map(token_lt, plesgreater), &
        token_map(token_gt, plesgreater), &
        token_map(token_plus, psum), &
        token_map(token_minus, psum), &
        token_map(token_slash, pproduct), &
        token_map(token_asterisk, pproduct), &
        token_map(token_lparen, pcall) &
    ]

    type :: parser_t
        type(lexer_t) :: l
        character(:), allocatable :: errors(:)
        type(token_t) :: cur_token
        type(token_t) :: peek_token
    end type parser_t
contains
    subroutine parser_init(l, p)
        type(lexer_t), intent(inout) :: l
        type(parser_t), intent(inout) :: p

        p%l = l
        call parser_next_token(p)
        call parser_next_token(p)
    end subroutine parser_init

    subroutine parser_next_token(p)
        type(parser_t), intent(inout) :: p
        p%cur_token = p%peek_token
        call lexer_next_token(p%l, p%peek_token)
    end subroutine parser_next_token

    function parser_cur_token_is(p, tok) result(b)
        type(parser_t), intent(in) :: p
        integer, intent(in) :: tok
        logical :: b
        b = tok == p%cur_token%type
    end function parser_cur_token_is

    function parser_peek_token_is(p, tok) result(b)
        type(parser_t), intent(in) :: p
        integer, intent(in) :: tok
        logical :: b
        b = tok == p%peek_token%type
    end function parser_peek_token_is

    function parser_expect_peek(p, tok) result(b)
        type(parser_t), intent(inout) :: p
        integer, intent(in) :: tok
        logical :: b

        if (parser_peek_token_is(p, tok)) then
            call parser_next_token(p)
            b = .true.
        else
            call parser_peek_error(p, tok)
            b = .false.
        end if
    end function parser_expect_peek

    function parser_peek_precedence(p) result(r)
        type(parser_t), intent(in) :: p
        integer :: i, r

        do i = 1, size(precedences)
            if (precedences(i)%key == p%peek_token%type) then
                r = precedences(i)%value
                return
            end if
        end do

        r = plowest
    end function parser_peek_precedence

    function parser_current_precedence(p) result(r)
        type(parser_t), intent(in) :: p
        integer :: i, r

        do i = 1, size(precedences)
            if (precedences(i)%key == p%cur_token%type) then
                r = precedences(i)%value
                return
            end if
        end do

        r = plowest
    end function parser_current_precedence

    subroutine parser_peek_error(p, tok)
        type(parser_t), intent(inout) :: p
        integer, intent(in) :: tok
        character(:), allocatable :: msg

        msg = "expected next token to be " // token_name(tok) // ", got " // token_name(p%peek_token%type)
        call parser_add_error(p, msg)
    end subroutine parser_peek_error

    subroutine parser_add_error(p, msg)
        type(parser_t), intent(inout) :: p
        character(:), allocatable, intent(in) :: msg

        if (allocated(p%errors)) then
            p%errors = [p%errors, msg]
        else
            p%errors = [msg]
        end if
    end subroutine parser_add_error

    subroutine parser_parse_program(p, prg)
        type(parser_t), intent(inout) :: p
        type(ast_program_t), intent(inout) :: prg
        class(*), allocatable :: s

        do while(p%cur_token%type /= token_eof)
            call parser_parse_stmt(p, s)
            if (allocated(s)) then
                call add_list_item(prg%head, s)
                deallocate(s)
            end if
            call parser_next_token(p)
        end do
    end subroutine parser_parse_program

    subroutine parser_parse_stmt(p, s)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: s

        select case(p%cur_token%type)
            case (token_let)
                call parser_parse_let_stmt(p, s)
            case (token_return)
                call parser_parse_return_stmt(p, s)
            case default
                call parser_parse_expr_stmt(p, s)
        end select
    end subroutine parser_parse_stmt

    subroutine parser_parse_let_stmt(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_let_t)  :: s

        s%tok = p%cur_token
        if (.not. parser_expect_peek(p, token_ident)) return

        s%name%tok = p%cur_token
        s%name%value = p%cur_token%literal

        if (.not. parser_expect_peek(p, token_assign)) return

        call parser_next_token(p)
        call parser_parse_expr(p, plowest, s%value)

        if (parser_peek_token_is(p, token_semicolon)) then
            call parser_next_token(p)
        end if

        r = s
    end subroutine parser_parse_let_stmt

    subroutine parser_parse_return_stmt(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_return_t) :: s

        s%tok = p%cur_token
        call parser_next_token(p)
        call parser_parse_expr(p, plowest, s%return_value)

        if (parser_peek_token_is(p, token_semicolon)) then
            call parser_next_token(p)
        end if

        r = s
    end subroutine parser_parse_return_stmt

    subroutine parser_parse_expr_stmt(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_expr_t) :: s

        s%tok = p%cur_token
        call parser_parse_expr(p, plowest, s%expr)

        if (parser_peek_token_is(p, token_semicolon)) then
            call parser_next_token(p)
        end if

        r = s
    end subroutine parser_parse_expr_stmt

    subroutine parser_parse_expr(p, precedence, s)
        type(parser_t), intent(inout) :: p
        integer, intent(in) :: precedence
        class(*), allocatable, intent(inout) :: s
        character(:), allocatable :: err

        select case(p%cur_token%type)
            case (token_ident)
                call parser_parse_identifier(p, s)
            case (token_int)
                call parser_parse_integer_literal(p, s)
            case (token_bang, token_minus)
                call parser_parse_prefix_expr(p, s)
            case (token_true, token_false)
                call parser_parse_bool_literal(p, s)
            case (token_lparen)
                call parser_parse_grouped_expr(p, s)
            case (token_if)
                call parser_parse_if_expr(p, s)
            case (token_function)
                call parser_parse_fn_lit(p, s)
            case default
                err = "expected expression got " // token_name(p%cur_token%type)
                call parser_add_error(p, err)
                return
        end select

        do while (precedence .lt. parser_peek_precedence(p) .and. .not. parser_peek_token_is(p, token_semicolon))
            select case (p%peek_token%type)
                case (&
                    token_plus, token_minus, token_slash, &
                    token_asterisk, token_eq, token_not_eq, &
                    token_lt, token_gt &
                )
                    call parser_next_token(p)
                    call parser_parse_infix_expr(p, s)
                case (token_lparen)
                    call parser_next_token(p)
                    call parser_parse_call_expr(p, s)
                case default
                    exit
            end select
        end do
    end subroutine parser_parse_expr

    subroutine parser_parse_identifier(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_ident_t) :: s

        s%tok = p%cur_token
        s%value = p%cur_token%literal

        r = s
    end subroutine parser_parse_identifier

    subroutine parser_parse_integer_literal(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_integer_t) :: s

        s%tok = p%cur_token
        read(p%cur_token%literal, *) s%value ! todo: handle read error

        r = s
    end subroutine parser_parse_integer_literal

    subroutine parser_parse_bool_literal(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_boolean_t) :: s

        s%tok = p%cur_token
        s%value = parser_cur_token_is(p, token_true)

        r = s
    end subroutine parser_parse_bool_literal

    subroutine parser_parse_prefix_expr(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_prefix_expr_t) :: s

        s%tok = p%cur_token
        s%op = p%cur_token%literal
        call parser_next_token(p)
        call parser_parse_expr(p, pprefix, s%right)

        r = s
    end subroutine parser_parse_prefix_expr

    subroutine parser_parse_grouped_expr(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r

        call parser_next_token(p)
        call parser_parse_expr(p, plowest, r)

        if (.not. parser_expect_peek(p, token_rparen)) then
            deallocate(r)
            return
        end if
    end subroutine parser_parse_grouped_expr

    subroutine parser_parse_if_expr(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_if_expr_t) :: s

        s%tok = p%cur_token
        if (.not. parser_expect_peek(p, token_lparen)) return

        call parser_next_token(p)
        call parser_parse_expr(p, plowest, s%condition)
        if (.not. parser_expect_peek(p, token_rparen)) return
        if (.not. parser_expect_peek(p, token_lbrace)) return

        call parser_parse_block_stmt(p, s%consequence)

        if (parser_peek_token_is(p, token_else)) then
            call parser_next_token(p)
            if (.not. parser_expect_peek(p, token_lbrace)) return
            call parser_parse_block_stmt(p, s%alternative)
        end if

        r = s
    end subroutine parser_parse_if_expr

    subroutine parser_parse_block_stmt(p, r)
        type(parser_t), intent(inout) :: p
        type(ast_block_t), allocatable, intent(inout) :: r
        class(*), allocatable :: ss
        type(ast_block_t) :: s

        s%tok = p%cur_token
        call parser_next_token(p)

        do while(p%cur_token%type /= token_rbrace .and. p%cur_token%type /= token_eof)
            call parser_parse_stmt(p, ss)
            if (allocated(ss)) then
                call add_list_item(s%stmts, ss)
                deallocate(ss)
            end if
            call parser_next_token(p)
        end do

        r = s
    end subroutine parser_parse_block_stmt

    subroutine parser_parse_fn_lit(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_fn_lit_t) :: s

        s%tok = p%cur_token
        if (.not. parser_expect_peek(p, token_lparen)) return

        call parser_parse_fn_params(p, s%params)
        if (.not. parser_expect_peek(p, token_lbrace)) return

        call parser_parse_block_stmt(p, s%body)

        r = s
    end subroutine parser_parse_fn_lit

    subroutine parser_parse_fn_params(p, s)
        type(parser_t), intent(inout) :: p
        type(ast_ident_t), allocatable, intent(inout) :: s(:)
        type(ast_ident_t) :: ident

        if (parser_peek_token_is(p, token_rparen)) then
            call parser_next_token(p)
            return
        end if

        call parser_next_token(p)

        ident%tok = p%cur_token
        ident%value = p%cur_token%literal
        s = [ident]

        do while(parser_peek_token_is(p, token_comma))
            call parser_next_token(p)
            call parser_next_token(p)
            ident%tok = p%cur_token
            ident%value = p%cur_token%literal
            s = [s, ident]
        end do

        if (.not. parser_expect_peek(p, token_rparen)) then
            deallocate(s)
        end if
    end subroutine parser_parse_fn_params

    subroutine parser_parse_infix_expr(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_infix_expr_t) :: s
        integer :: precedence

        s%tok = p%cur_token
        s%op = p%cur_token%literal
        s%left = r

        precedence = parser_current_precedence(p)
        call parser_next_token(p)
        call parser_parse_expr(p, precedence, s%right)
        deallocate(r)
        r = s
    end subroutine parser_parse_infix_expr

    subroutine parser_parse_call_expr(p, r)
        type(parser_t), intent(inout) :: p
        class(*), allocatable, intent(inout) :: r
        type(ast_call_expr_t) :: s

        s%tok = p%cur_token
        s%fn = r
        call parser_parse_call_args(p, s%args)

        if (allocated(r)) deallocate(r)
        r = s
    end subroutine parser_parse_call_expr

    subroutine parser_parse_call_args(p, r)
        type(parser_t), intent(inout) :: p
        type(list_t), pointer, intent(out) :: r
        class(*), allocatable :: ss

        type(list_t), pointer :: current

        if (parser_peek_token_is(p, token_rparen)) then
            call parser_next_token(p)
            return
        end if

        call parser_next_token(p)
        call parser_parse_expr(p, plowest, ss)
        if (allocated(ss)) then
            call add_list_item(r, ss)
        endif

        do while (parser_peek_token_is(p, token_comma))
            call parser_next_token(p)
            call parser_next_token(p)
            call parser_parse_expr(p, plowest, ss)
            if (allocated(ss)) then
                call add_list_item(r, ss)
            end if
        end do

        if (.not. parser_expect_peek(p, token_rparen)) then
            r => null ()
        end if
    end subroutine parser_parse_call_args
end module parser
