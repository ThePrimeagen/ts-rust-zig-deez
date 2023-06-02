module ast
    use list
    use token

    implicit none

    type :: ast_program_t
        type(list_t), pointer :: head => null ()
    end type ast_program_t

    type :: ast_ident_t
        type(token_t) :: tok
        character(:), allocatable :: value
    end type ast_ident_t

    type :: ast_let_t
        type(token_t) :: tok
        type(ast_ident_t) :: name
        class(*), allocatable :: value
    end type ast_let_t

    type :: ast_return_t
        type(token_t) :: tok
        class(*), allocatable :: return_value
    end type ast_return_t

    type :: ast_expr_t
        type(token_t) :: tok
        class(*), allocatable :: expr
    end type ast_expr_t

    type :: ast_block_t
        type(token_t) :: tok
        type(list_t), pointer :: stmts => null()
    end type ast_block_t

    type :: ast_boolean_t
        type(token_t) :: tok
        logical :: value
    end type ast_boolean_t

    type :: ast_integer_t
        type(token_t) :: tok
        integer :: value
    end type ast_integer_t

    type :: ast_prefix_expr_t
        type(token_t) :: tok
        character(:), allocatable :: op
        class(*), allocatable :: right
    end type ast_prefix_expr_t

    type :: ast_infix_expr_t
        type(token_t) :: tok
        class(*), allocatable :: left
        character(:), allocatable :: op
        class(*), allocatable :: right
    end type ast_infix_expr_t

    type :: ast_if_expr_t
        type(token_t) :: tok
        class(*), allocatable :: condition
        type(ast_block_t), allocatable :: consequence
        type(ast_block_t), allocatable :: alternative
    end type ast_if_expr_t

    type :: ast_fn_lit_t
        type(token_t) :: tok
        type(ast_ident_t), allocatable :: params(:)
        type(ast_block_t), allocatable :: body
    end type ast_fn_lit_t

    type :: ast_call_expr_t
        type(token_t) :: tok
        class(*), allocatable :: fn
        type(list_t), pointer :: args => null ()
    end type ast_call_expr_t
contains
    recursive function ast_token_literal(self) result(s)
        class(*), intent(in) :: self
        character(:), allocatable :: s

        select type(self)
            type is (ast_program_t)
                if (associated(self%head)) then
                    s = ast_token_literal(self%head%item)
                else
                    s = ""
                end if
            type is (ast_ident_t)
                s = self%tok%literal
            type is (ast_let_t)
                s = self%tok%literal
            type is (ast_return_t)
                s = self%tok%literal
            type is (ast_expr_t)
                s = self%tok%literal
            type is (ast_block_t)
                s = self%tok%literal
            type is (ast_boolean_t)
                s = self%tok%literal
            type is (ast_integer_t)
                s = self%tok%literal
            type is (ast_prefix_expr_t)
                s = self%tok%literal
            type is (ast_infix_expr_t)
                s = self%tok%literal
            type is (ast_if_expr_t)
                s = self%tok%literal
            type is (ast_fn_lit_t)
                s = self%tok%literal
            type is (ast_call_expr_t)
                s = self%tok%literal
            class default
                call backtrace
                stop 1
        end select
    end function ast_token_literal

    recursive function ast_string(self) result(s)
        class(*), intent(in) :: self
        character(:), allocatable :: s

        select type(self)
            type is (ast_ident_t)
                s = ast_ident_string(self)
            type is (ast_let_t)
                s = ast_let_string(self)
            type is (ast_return_t)
                s = ast_return_string(self)
            type is (ast_expr_t)
                s = ast_expr_string(self)
            type is (ast_block_t)
                s = ast_block_string(self)
            type is (ast_boolean_t)
                s = ast_boolean_string(self)
            type is (ast_integer_t)
                s = ast_integer_string(self)
            type is (ast_prefix_expr_t)
                s = ast_prefix_expr_string(self)
            type is (ast_infix_expr_t)
                s = ast_infix_expr_string(self)
            type is (ast_if_expr_t)
                s = ast_if_expr_string(self)
            type is (ast_fn_lit_t)
                s = ast_fn_lit_string(self)
            type is (ast_call_expr_t)
                s = ast_call_expr_string(self)
            type is (ast_program_t)
                s = ast_program_string(self)
            class default
                call backtrace
                stop 1
        end select
    end function ast_string

    function ast_program_string(self) result(s)
        type(ast_program_t), intent(in) :: self
        character(:), allocatable :: s
        type(list_t), pointer :: current => null ()

        s = ""
        current => self%head
        do while (associated(current))
            s = s // ast_string(current%item)
            current => current%next
        end do
    end function ast_program_string

    function ast_ident_string(self) result(s)
        type(ast_ident_t), intent(in) :: self
        character(:), allocatable :: s
        s = self%value
    end function ast_ident_string

    function ast_let_string(self) result(s)
        type(ast_let_t), intent(in) :: self
        character(:), allocatable :: s
        s = self%tok%literal // " " // ast_string(self%name) // " = "
        if (allocated(self%value)) then
            s = s // ast_string(self%value)
        end if
        s = s // ";"
    end function ast_let_string

    function ast_return_string(self) result(s)
        type(ast_return_t), intent(in) :: self
        character(:), allocatable :: s
        s = self%tok%literal // " "
        if (allocated(self%return_value)) then
            s = s // ast_string(self%return_value)
        end if
        s = s // ";"
    end function ast_return_string

    function ast_expr_string(self) result(s)
        type(ast_expr_t), intent(in) :: self
        character(:), allocatable :: s
        if (allocated(self%expr)) then
            s = ast_string(self%expr)
        else
            s = ""
        end if
    end function ast_expr_string

    function ast_block_string(self) result(s)
        type(ast_block_t), intent(in) :: self
        character(:), allocatable :: s
        type(list_t), pointer :: current => null ()

        s = ""
        current => self%stmts
        do while(associated(current))
            s = s // ast_string(current%item)
            current => current%next
        end do
    end function ast_block_string

    function ast_boolean_string(self) result(s)
        type(ast_boolean_t), intent(in) :: self
        character(:), allocatable :: s
        s = self%tok%literal
    end function ast_boolean_string

    function ast_integer_string(self) result(s)
        type(ast_integer_t), intent(in) :: self
        character(:), allocatable :: s
        s = self%tok%literal
    end function ast_integer_string

    function ast_prefix_expr_string(self) result(s)
        type(ast_prefix_expr_t), intent(in) :: self
        character(:), allocatable :: s
        s = "(" // self%op // ast_string(self%right) // ")"
    end function ast_prefix_expr_string

    recursive function ast_infix_expr_string(self) result(s)
        type(ast_infix_expr_t), intent(in) :: self
        character(:), allocatable :: s
        character(:), allocatable :: left, right

        ! This line does not work but the code below this line works. They're basically the same thing. Wtf Fortran?
        ! s = "(" // ast_string(self%left) // " " // self%op // " " // ast_string(self%right) // ")"

        left = ast_string(self%left) // " "
        right = " " // ast_string(self%right)
        s = "(" // left // self%op // right // ")"
    end function ast_infix_expr_string

    function ast_if_expr_string(self) result(s)
        type(ast_if_expr_t), intent(in) :: self
        character(:), allocatable :: s
        s = "if " // ast_string(self%condition) // " " // ast_string(self%consequence)
        if (allocated(self%alternative)) then
            s = s // " else " // ast_string(self%alternative)
        end if
    end function ast_if_expr_string

    function ast_fn_lit_string(self) result(s)
        type(ast_fn_lit_t), intent(in) :: self
        character(:), allocatable :: s
        integer :: i
        s = self%tok%literal // "("
        if (allocated(self%params) .and. size(self%params) > 0) then
            s = s // ast_string(self%params(1))
            do i = 2, size(self%params)
                s = s // ", " // ast_string(self%params(i))
            end do
        end if
        s = s // ") " // ast_string(self%body)
    end function ast_fn_lit_string

    function ast_call_expr_string(self) result(s)
        type(ast_call_expr_t), intent(in) :: self
        character(:), allocatable :: s
        type(list_t), pointer :: current => null ()
        s = ast_string(self%fn) // "("
        current => self%args
        if (associated(current)) then
            s = s // ast_string(current%item)
            do while (associated(current%next))
                current => current%next
                s = s // ", " // ast_string(current%item)
            end do
        end if
        s = s // ")"
    end function ast_call_expr_string
end module ast
