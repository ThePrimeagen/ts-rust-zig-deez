module list
    implicit none

    type :: list_t
        class(*), allocatable :: item
        type(list_t), pointer :: next => null()
    end type list_t
contains
    subroutine add_list_item(head, item)
        type(list_t), intent(inout), pointer :: head
        class(*), intent(in) :: item
        type(list_t), pointer :: current => null ()
        type(list_t), pointer :: new_item => null ()

        current => head
        if (associated(head)) then
            do while (associated(current%next))
                current => current%next
            end do

            allocate(new_item)
            new_item%item = item
            current%next => new_item
        else
            allocate(head)
            head%item = item
        end if
    end subroutine

    function list_size(head) result(size)
        type(list_t), intent(in), pointer :: head
        type(list_t), pointer :: current => null ()
        integer :: size

        size = 0
        current => head
        do while (associated(current))
            size = size + 1
            current => current%next
        end do
    end function
end module list
