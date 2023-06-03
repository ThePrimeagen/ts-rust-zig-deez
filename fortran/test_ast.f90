program test_ast
    use list
    use token
    use ast

    implicit none

    type(ast_program_t) :: prg
    character(:), allocatable :: expected
    type(ast_let_t) :: let
    type(ast_ident_t), target :: value

    expected = "let myVar = anotherVar;"

    let%tok = token_t(token_let, "let")
    let%name = ast_ident_t(token_t(token_ident, "myVar"), "myVar")
    value = ast_ident_t(token_t(token_ident, "anotherVar"), "anotherVar")
    let%value = value

    call add_list_item(prg%head, let)

    if (ast_string(prg) /= expected) then
        print *, "Expected: ", expected
        print *, "Got: ", ast_string(prg)
        stop 1
    end if

    print *, "all tests passed"
end program test_ast
