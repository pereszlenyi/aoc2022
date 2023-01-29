program most_calories
    implicit none

    ! Variables
    integer, parameter :: TOP_ARR_LENGTH = 4
    character(len=100) :: input_file, line
    integer :: file_unit, io_status, current_sum, i
    integer :: top_calories(TOP_ARR_LENGTH)

    ! Executable statements
    if (command_argument_count() /= 1) call die("The input file must be given as a command line parameter.")

    current_sum = 0
    do i = 1, TOP_ARR_LENGTH
        top_calories(i) = 0
    end do

    call get_command_argument(1, input_file)
    open(newunit=file_unit, file=trim(input_file), status="old")
    do
        read(file_unit, '(A)', iostat=io_status) line
        if (io_status == 0) then
            if (trim(line) == "") then
                top_calories(TOP_ARR_LENGTH) = current_sum
                call sort(top_calories)
                current_sum = 0
            else
                current_sum = current_sum + str_to_int(trim(line))
            end if
        else
            exit
        end if
    end do
    close(file_unit)

    top_calories(TOP_ARR_LENGTH) = current_sum
    call sort(top_calories)

    current_sum = 0
    do i = 1, TOP_ARR_LENGTH - 1
        current_sum = current_sum + top_calories(i)
    end do

    print '("The most Calories an Elf carries is ", I0)', top_calories(1)
    print '("The sum of the Calories the top three Elves carry is ", I0)', current_sum

contains
    ! Prints the error message and terminates the program.
    subroutine die(error_message)
        character(len=*), intent(in) :: error_message

        print '("Error: ", A)', error_message
        stop 1
    end subroutine die

    ! Converts a string to an integer.
    function str_to_int(str) result(int)
        character(len=*), intent(in) :: str
        integer :: int

        integer :: io_status

        read(str, *, iostat = io_status) int
        if (io_status /= 0) call die("Couldn't convert '" // str // "' to integer.")
    end function str_to_int

    ! Sorts an array of integers with size TOP_ARR_LENGTH
    subroutine sort(array)
        integer, intent(inout) :: array(TOP_ARR_LENGTH)

        integer :: i, j, temp

        do i = 1, TOP_ARR_LENGTH - 1
            do j = TOP_ARR_LENGTH, i + 1, -1
                if (array(j - 1) < array(j)) then
                    temp = array(j)
                    array(j) = array(j - 1)
                    array(j - 1) = temp
                end if
            end do
        end do
    end subroutine sort
end program most_calories
