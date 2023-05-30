program main
    use lexer
    use token

    implicit none

    character(:), allocatable :: input
    type(token_t), allocatable :: expected(:)
    type(lexer_t) :: l
    type(token_t) :: tok
    integer :: i

    input = "let five = 5;&
let ten = 10;&

let add = fn(x, y) {&
  x + y;&
};&

let result = add(five, ten);&
-!/*5;&
5 < 10 > 5;&

if (5 < 10) {&
	return true;&
} else {&
	return false;&
}&

10 == 10;&
10 != 9;"

    expected = [&
        token_t(token_let, "let"), &
        token_t(token_ident, "five"), &
        token_t(token_assign, "="), &
        token_t(token_int, "5"), &
        token_t(token_semicolon, ";"), &
        token_t(token_let, "let"), &
        token_t(token_ident, "ten"), &
        token_t(token_assign, "="), &
        token_t(token_int, "10"), &
        token_t(token_semicolon, ";"), &
        token_t(token_let, "let"), &
        token_t(token_ident, "add"), &
        token_t(token_assign, "="), &
        token_t(token_function, "fn"), &
        token_t(token_lparen, "("), &
        token_t(token_ident, "x"), &
        token_t(token_comma, ","), &
        token_t(token_ident, "y"), &
        token_t(token_rparen, ")"), &
        token_t(token_LBRACE, "{"), &
        token_t(token_ident, "x"), &
        token_t(token_plus, "+"), &
        token_t(token_ident, "y"), &
        token_t(token_semicolon, ";"), &
        token_t(token_rbrace, "}"), &
        token_t(token_semicolon, ";"), &
        token_t(token_let, "let"), &
        token_t(token_ident, "result"), &
        token_t(token_assign, "="), &
        token_t(token_ident, "add"), &
        token_t(token_lparen, "("), &
        token_t(token_ident, "five"), &
        token_t(token_comma, ","), &
        token_t(token_ident, "ten"), &
        token_t(token_rparen, ")"), &
        token_t(token_semicolon, ";"), &
        token_t(token_minus, "-"), &
        token_t(token_bang, "!"), &
        token_t(token_slash, "/"), &
        token_t(token_asterisk, "*"), &
        token_t(token_int, "5"), &
        token_t(token_semicolon, ";"), &
        token_t(token_int, "5"), &
        token_t(token_lt, "<"), &
        token_t(token_int, "10"), &
        token_t(token_gt, ">"), &
        token_t(token_int, "5"), &
        token_t(token_semicolon, ";"), &
        token_t(token_if, "if"), &
        token_t(token_lparen, "("), &
        token_t(token_int, "5"), &
        token_t(token_lt, "<"), &
        token_t(token_int, "10"), &
        token_t(token_rparen, ")"), &
        token_t(token_lbrace, "{"), &
        token_t(token_return, "return"), &
        token_t(token_true, "true"), &
        token_t(token_semicolon, ";"), &
        token_t(token_rbrace, "}"), &
        token_t(token_else, "else"), &
        token_t(token_lbrace, "{"), &
        token_t(token_return, "return"), &
        token_t(token_false, "false"), &
        token_t(token_semicolon, ";"), &
        token_t(token_rbrace, "}"), &
        token_t(token_int, "10"), &
        token_t(token_eq, "=="), &
        token_t(token_int, "10"), &
        token_t(token_semicolon, ";"), &
        token_t(token_int, "10"), &
        token_t(token_not_eq, "!="), &
        token_t(token_int, "9"), &
        token_t(token_semicolon, ";"), &
        token_t(token_eof, "") &
    ]

    call lexer_init(l, input)
    do i = 1, size(expected)
        call lexer_next_token(l, tok)

        if (tok%type /= expected(i)%type) then
            print *, "test failed", i
            print *, "expected: ", expected(i)%type, expected(i)%literal
            print *, "got: ", tok%type, tok%literal
            stop
        end if
    end do

    print *, "all tests passed"
end program main
