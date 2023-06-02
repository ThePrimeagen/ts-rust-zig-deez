program test_parser
    use list
    use token
    use lexer
    use ast
    use parser

    implicit none

    type(lexer_t) :: l
    type(parser_t) :: p
    type(ast_program_t) :: prg

    call init_test("let x = 5;", l, p, prg)
    call expect_let_stmt(prg%head%item, "x", 5)

    call init_test("let y = true;", l, p, prg)
    call expect_let_stmt(prg%head%item, "y", .true.)

    call init_test("let foobar = y;", l, p, prg)
    call expect_let_stmt(prg%head%item, "foobar", "y")

    call init_test("return 5;", l, p, prg)
    call expect_return_stmt(prg%head%item, 5)

    call init_test("return false;", l, p, prg)
    call expect_return_stmt(prg%head%item, .false.)

    call init_test("return foobar;", l, p, prg)
    call expect_return_stmt(prg%head%item, "foobar")

    call init_test("foobar", l, p, prg)
    call expect_expr_stmt(prg%head%item, "foobar")

    call init_test("5", l, p, prg)
    call expect_expr_stmt(prg%head%item, 5)

    call init_test("!5;", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "!", 5)

    call init_test("-15;", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "-", 15)

    call init_test("!foobar", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "!", "foobar")

    call init_test("-foobar", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "-", "foobar")

    call init_test("!true", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "!", .true.)

    call init_test("!false", l, p, prg)
    call expect_prefix_expr_stmt(prg%head%item, "!", .false.)

    call init_test("5 + 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "+", 5)

    call init_test("5 - 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "-", 5)

    call init_test("5 * 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "*", 5)

    call init_test("5 / 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "/", 5)

    call init_test("5 > 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, ">", 5)

    call init_test("5 < 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "<", 5)

    call init_test("5 == 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "==", 5)

    call init_test("5 != 5;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, 5, "!=", 5)

    call init_test("foobar + barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "+", "barfoo")

    call init_test("foobar - barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "-", "barfoo")

    call init_test("foobar * barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "*", "barfoo")

    call init_test("foobar / barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "/", "barfoo")

    call init_test("foobar > barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", ">", "barfoo")

    call init_test("foobar < barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "<", "barfoo")

    call init_test("foobar == barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "==", "barfoo")

    call init_test("foobar != barfoo;", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, "foobar", "!=", "barfoo")

    call init_test("true == true", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, .true., "==", .true.)

    call init_test("true != false", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, .true., "!=", .false.)

    call init_test("false == false", l, p, prg)
    call expect_infix_expr_stmt(prg%head%item, .false., "==", .false.)

    call init_test("-a * b", l, p, prg)
    call expect_ast_string(prg, "((-a) * b)")

    call init_test("!-a", l, p, prg)
    call expect_ast_string(prg, "(!(-a))")

    call init_test("a + b + c", l, p, prg)
    call expect_ast_string(prg, "((a + b) + c)")

    call init_test("a + b - c", l, p, prg)
    call expect_ast_string(prg, "((a + b) - c)")

    call init_test("a * b * c", l, p, prg)
    call expect_ast_string(prg, "((a * b) * c)")

    call init_test("a * b / c", l, p, prg)
    call expect_ast_string(prg, "((a * b) / c)")

    call init_test("a + b / c", l, p, prg)
    call expect_ast_string(prg, "(a + (b / c))")

    call init_test("a + b * c + d / e - f", l, p, prg)
    call expect_ast_string(prg, "(((a + (b * c)) + (d / e)) - f)")

    call init_test("3 + 4; -5 * 5", l, p, prg, 2)
    call expect_ast_string(prg, "(3 + 4)((-5) * 5)")

    call init_test("5 > 4 == 3 < 4", l, p, prg)
    call expect_ast_string(prg, "((5 > 4) == (3 < 4))")

    call init_test("5 < 4 != 3 > 4", l, p, prg)
    call expect_ast_string(prg, "((5 < 4) != (3 > 4))")

    call init_test("3 + 4 * 5 == 3 * 1 + 4 * 5", l, p, prg)
    call expect_ast_string(prg, "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")

    call init_test("true", l, p, prg)
    call expect_ast_string(prg, "true")

    call init_test("false", l, p, prg)
    call expect_ast_string(prg, "false")

    call init_test("3 > 5 == false", l, p, prg)
    call expect_ast_string(prg, "((3 > 5) == false)")

    call init_test("3 < 5 == true", l, p, prg)
    call expect_ast_string(prg, "((3 < 5) == true)")

    call init_test("1 + (2 + 3) + 4", l, p, prg)
    call expect_ast_string(prg, "((1 + (2 + 3)) + 4)")

    call init_test("(5 + 5) * 2", l, p, prg)
    call expect_ast_string(prg, "((5 + 5) * 2)")

    call init_test("2 / (5 + 5)", l, p, prg)
    call expect_ast_string(prg, "(2 / (5 + 5))")

    call init_test("-(5 + 5)", l, p, prg)
    call expect_ast_string(prg, "(-(5 + 5))")

    call init_test("!(true == true)", l, p, prg)
    call expect_ast_string(prg, "(!(true == true))")

    call init_test("a + add(b * c) + d", l, p, prg)
    call expect_ast_string(prg, "((a + add((b * c))) + d)")

    call init_test("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", l, p, prg)
    call expect_ast_string(prg, "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")

    call init_test("add(a + b + c * d / f + g)", l, p, prg)
    call expect_ast_string(prg, "add((((a + b) + ((c * d) / f)) + g))")

    print *, "all tests passed"
contains
    subroutine init_test(input, l, p, prg, num_stms)
        character(len=*), intent(in) :: input
        type(lexer_t), intent(out) :: l
        type(parser_t), intent(out) :: p
        type(ast_program_t), intent(out) :: prg
        character(:), allocatable :: i
        integer, optional, intent(in) :: num_stms

        i = input
        call lexer_init(l, i)
        call parser_init(l, p)
        call parser_parse_program(p, prg)
        call expect_no_parse_errors(p)
        if (present(num_stms)) then
            call expect_smt_size(prg, num_stms)
        else
            call expect_smt_size(prg, 1)
        end if
    end subroutine init_test

    subroutine expect_no_parse_errors(p)
        type(parser_t), intent(in) :: p
        integer :: i

        if (len(p%errors) > 0) then
            do i = 1, size(p%errors)
                print *, p%errors(i)
            end do
            stop 1
        end if
    end subroutine expect_no_parse_errors

    subroutine expect_smt_size(prg, size)
        class(ast_program_t), intent(in) :: prg
        integer, intent(in) :: size
        integer :: s

        s = list_size(prg%head)
        if (s /= size) then
            print *, "expected ", size, " statements, got ", s
            stop 1
        end if
    end subroutine expect_smt_size

    subroutine expect_let_stmt(self, ident, value)
        class(*), intent(in), allocatable :: self
        character(len=*), intent(in) :: ident
        class(*), intent(in) :: value

        select type(self)
            type is (ast_let_t)
                if (ast_token_literal(self%name) /= ident) then
                    print *, "expected identifier ", ident, " got ", ast_token_literal(self%name)
                    stop 1
                end if
                call expect_literal_value(self%value, value)
            class default
                print *, "expected let statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine expect_let_stmt

    subroutine expect_return_stmt(self, value)
        class(*), intent(in), allocatable :: self
        class(*), intent(in) :: value

        select type(self)
            type is (ast_return_t)
                call expect_literal_value(self%return_value, value)
            class default
                print *, "expected return statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine expect_return_stmt

    subroutine expect_expr_stmt(self, value)
        class(*), intent(in), allocatable :: self
        class(*), intent(in) :: value

        select type(self)
            type is (ast_expr_t)
                call expect_literal_value(self%expr, value)
            class default
                print *, "expected identifier statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine expect_expr_stmt

    subroutine expect_prefix_expr_stmt(self, operator, value)
        class(*), intent(in), allocatable :: self
        character(len=*) :: operator
        class(*), intent(in) :: value

        select type(self)
            type is(ast_expr_t)
                call test_prefix_expr_stmt(self%expr, operator, value)
            class default
                print *, "expected expression statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine expect_prefix_expr_stmt

    subroutine test_prefix_expr_stmt(self, operator, value)
        class(*), intent(in) :: self
        character(len=*) :: operator
        class(*), intent(in) :: value

        select type(self)
            type is(ast_prefix_expr_t)
                if (self%op /= operator) then
                    print *, "expected operator ", operator, " got ", self%op
                    stop 1
                end if
                call expect_literal_value(self%right, value)
            class default
                print *, "expected prefix expression statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine

    subroutine expect_infix_expr_stmt(self, left, op, right)
        class(*), intent(in) :: self, left, right
        character(len=*), intent(in) :: op

        select type(self)
            type is(ast_expr_t)
                call test_infix_expr_stmt(self%expr, left, op, right)
            class default
                print *, "expected expression statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine expect_infix_expr_stmt

    subroutine test_infix_expr_stmt(self, left, op, right)
        class(*), intent(in) :: self, left, right
        character(len=*), intent(in) :: op

        select type(self)
            type is(ast_infix_expr_t)
                if (self%op /= op) then
                    print *, "expected operator ", op, " got ", self%op
                    stop 1
                end if
                call expect_literal_value(self%left, left)
                call expect_literal_value(self%right, right)
            class default
                print *, "expected infix expression statement, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine test_infix_expr_stmt

    subroutine expect_ast_string(self, expected)
        class(*), intent(in) :: self
        character(len=*), intent(in) :: expected
        character(:), allocatable :: s

        s = ast_string(self)
        if (s /= expected) then
            print *, "expected ", expected, " got ", s
            stop 1
        end if
    end subroutine

    subroutine expect_literal_value(self, value)
        class(*), intent(in) :: self
        class(*), intent(in) :: value

        select type(value)
            type is(integer)
                call test_interger_literal(self, value)
            type is (character(len=*))
                call test_ident_literal(self, value)
            type is (logical)
                call test_bool_literal(self, value)
            class default
                print *, "literal type not supported"
                stop 1
        end select
    end subroutine expect_literal_value

    subroutine test_interger_literal(self, value)
        class(*), intent(in) :: self
        integer, intent(in) :: value

        select type(self)
            type is(ast_integer_t)
                if (self%value /= value) then
                    print *, "expected int ", value, " got ", self%value
                    stop 1
                end if
            class default
                print *, "expected integer literal, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine test_interger_literal

    subroutine test_ident_literal(self, value)
        class(*), intent(in) :: self
        character(len=*), intent(in) :: value

        select type(self)
            type is (ast_ident_t)
                if (self%value /= value) then
                    print *, "expected identifier ", value, " got ", self%value
                    stop 1
                end if
            class default
                print *, "expected identifer, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine test_ident_literal

    subroutine test_bool_literal(self, value)
        class(*), intent(in) :: self
        logical, intent(in) :: value

        select type(self)
            type is (ast_boolean_t)
                if (self%value .neqv. value) then
                    print *, "expected bool ", value, " got ", self%value
                    stop 1
                end if
            class default
                print *, "expected bool, got ", ast_token_literal(self)
                stop 1
        end select
    end subroutine test_bool_literal
end program test_parser
