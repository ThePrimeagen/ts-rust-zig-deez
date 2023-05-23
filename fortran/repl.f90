program main
    use lexer
    use token

    implicit none

    character(len=1024) :: line
    integer :: size
    character(len=:), allocatable :: input
    type(lexer_t), allocatable :: lex
    type(token_t), allocatable :: tok

    write(*, *) "Type .exit to exit the repl"
    do
        write(*, '(A)', advance="no") ">> "
        read(*, '(A)') line

        line = trim(line)
        if (line == ".exit" .or. line == ".quit") then
            exit
        else if (line /= "") then
            size = len_trim(line)
            allocate(character(size) :: input)
            allocate(tok)
            allocate(lex)
            input = line(1:size)
            call lexer_init(lex, input)
            do
                call lexer_next_token(lex, tok)
                size = len_trim(tok%literal)
                write(*, *) token_name(tok%type), " = " , tok%literal(1:size)
                if (tok%type == token_eof) exit
            end do
            deallocate(input)
            deallocate(tok)
            deallocate(lex)
        end if
    end do
end program main
