program main
    use list
    use lexer
    use token
    use ast
    use parser

    implicit none

    character(len=1024) :: line
    character(:), allocatable :: input
    integer :: size

    write(*, *) "Type .exit to exit the repl"
    do
        write(*, '(A)', advance="no") ">> "
        read(*, '(A)') line

        line = trim(line)
        if (line == ".exit" .or. line == ".quit") then
            exit
        else if (line /= "") then
            size = len_trim(line)
            input = line(1:size)
            call run_line(input)
        end if
    end do
contains
    subroutine run_line(input)
        character(:), allocatable, intent(in) :: input
        type(lexer_t) :: l
        type(parser_t) :: p
        type(ast_program_t) :: prg

        call lexer_init(l, input)
        call parser_init(l, p)
        call parser_parse_program(p, prg)

        if (allocated(p%errors)) then
            call print_parser_errors(p)
        else
            write(*, *) ast_string(prg)
        end if
    end subroutine run_line

    subroutine print_parser_errors(p)
        type(parser_t), intent(in) :: p
        integer :: i

        write(*, *) "no banana for you!"
        do i = 1, count(p%errors /= "")
            write(*, *) trim(p%errors(i))
        end do
    end subroutine print_parser_errors
end program main
