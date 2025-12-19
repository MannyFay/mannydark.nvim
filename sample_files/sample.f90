! ==============================================================================
! Comprehensive Fortran Sample - Syntax Highlighting Demonstration
! ==============================================================================

! This file demonstrates all major Modern Fortran language features
! (Fortran 90/95/2003/2008/2018) for syntax highlighting purposes.

! ==============================================================================
! Comments
! ==============================================================================

! This is a single-line comment
!! Double exclamation for documentation comments

! ==============================================================================
! Program Structure
! ==============================================================================

program sample_fortran
    ! Use statements
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: my_module, only: my_subroutine, my_type

    implicit none  ! Always use implicit none!

    ! ==============================================================================
    ! Variable Declarations
    ! ==============================================================================

    ! Integer types
    integer :: i, j, k
    integer(kind=1) :: int8_val      ! 1-byte integer
    integer(kind=2) :: int16_val     ! 2-byte integer
    integer(kind=4) :: int32_val     ! 4-byte integer
    integer(kind=8) :: int64_val     ! 8-byte integer
    integer(int8) :: iso_int8        ! Using iso_fortran_env constants
    integer(int16) :: iso_int16
    integer(int32) :: iso_int32
    integer(int64) :: iso_int64

    ! Real (floating-point) types
    real :: single_precision
    real(kind=4) :: float32
    real(kind=8) :: float64          ! Double precision
    real(kind=16) :: float128        ! Quad precision (if available)
    real(real32) :: iso_float32
    real(real64) :: iso_float64
    double precision :: dp_value     ! Legacy double precision

    ! Complex types
    complex :: complex_single
    complex(kind=8) :: complex_double
    complex(real64) :: iso_complex

    ! Logical type
    logical :: flag, is_valid
    logical(kind=1) :: small_flag

    ! Character types
    character :: single_char
    character(len=10) :: fixed_string
    character(len=*), parameter :: const_string = "Hello, Fortran!"
    character(len=:), allocatable :: dynamic_string

    ! Parameters (constants)
    real, parameter :: pi = 3.14159265358979_real64
    integer, parameter :: max_size = 1000
    character(len=*), parameter :: app_name = "Fortran Demo"

    ! Arrays
    integer :: static_array(10)
    integer :: multi_dim(3, 4, 5)
    real :: matrix(10, 10)
    integer, dimension(100) :: vec
    integer, allocatable :: dynamic_array(:)
    integer, allocatable :: alloc_matrix(:,:)
    real, pointer :: ptr_array(:)

    ! Array with explicit bounds
    real :: array_bounds(-5:5)
    real :: offset_array(0:99)

    ! Derived types (structures)
    type :: person_type
        character(len=50) :: name
        integer :: age
        real :: height
    end type person_type

    type(person_type) :: person
    type(person_type), dimension(100) :: people

    ! Pointer variables
    integer, pointer :: int_ptr => null()
    real, pointer :: matrix_ptr(:,:) => null()
    type(person_type), pointer :: person_ptr

    ! Target attribute for pointer assignment
    integer, target :: target_var
    real, target :: target_array(100)

    ! Procedure pointers
    procedure(my_interface), pointer :: proc_ptr => null()

    ! Coarray (Fortran 2008)
    integer :: coarray_var[*]
    real, allocatable :: coarray_alloc(:)[:]

    ! ==============================================================================
    ! Literal Values
    ! ==============================================================================

    i = 42                      ! Integer
    i = -17                     ! Negative integer
    single_precision = 3.14     ! Real
    float64 = 3.14159_real64    ! Real with kind specifier
    float64 = 3.14d0            ! Double precision literal
    float64 = 6.022e23          ! Scientific notation
    float64 = 1.0d-10           ! Double precision scientific
    complex_single = (1.0, 2.0) ! Complex literal
    flag = .true.               ! Logical true
    flag = .false.              ! Logical false
    fixed_string = "Hello"      ! String literal
    fixed_string = 'World'      ! Also valid string

    ! Binary, octal, hexadecimal
    i = B"11010110"             ! Binary
    i = O"755"                  ! Octal
    i = Z"DEADBEEF"             ! Hexadecimal

    ! BOZ literals (Fortran 2008)
    i = int(B'11010110')
    i = int(O'755')
    i = int(Z'FF')

    ! ==============================================================================
    ! Operators
    ! ==============================================================================

    ! Arithmetic operators
    i = 10 + 5                  ! Addition
    i = 10 - 5                  ! Subtraction
    i = 10 * 5                  ! Multiplication
    i = 10 / 5                  ! Division
    i = 10 ** 2                 ! Exponentiation

    ! Relational operators (Fortran 90+)
    flag = (i == j)             ! Equal
    flag = (i /= j)             ! Not equal
    flag = (i < j)              ! Less than
    flag = (i <= j)             ! Less than or equal
    flag = (i > j)              ! Greater than
    flag = (i >= j)             ! Greater than or equal

    ! Legacy relational operators (still valid)
    flag = (i .eq. j)
    flag = (i .ne. j)
    flag = (i .lt. j)
    flag = (i .le. j)
    flag = (i .gt. j)
    flag = (i .ge. j)

    ! Logical operators
    flag = .not. flag
    flag = flag .and. is_valid
    flag = flag .or. is_valid
    flag = flag .eqv. is_valid  ! Equivalent (XNOR)
    flag = flag .neqv. is_valid ! Not equivalent (XOR)

    ! String concatenation
    fixed_string = "Hello" // " " // "World"

    ! ==============================================================================
    ! Control Flow
    ! ==============================================================================

    ! IF-THEN-ELSE
    if (i > 0) then
        print *, "Positive"
    else if (i < 0) then
        print *, "Negative"
    else
        print *, "Zero"
    end if

    ! Single-line IF
    if (i > 0) print *, "Positive"

    ! Named IF construct
    check: if (is_valid) then
        print *, "Valid"
    else check
        print *, "Invalid"
    end if check

    ! SELECT CASE
    select case (i)
        case (1)
            print *, "One"
        case (2, 3, 4)
            print *, "Two, Three, or Four"
        case (5:10)
            print *, "Five to Ten"
        case (:-1)
            print *, "Negative"
        case default
            print *, "Other"
    end select

    ! Named SELECT CASE
    named_select: select case (fixed_string)
        case ("hello")
            print *, "Greeting"
        case default
            print *, "Unknown"
    end select named_select

    ! SELECT TYPE (Fortran 2003)
    ! select type (poly_var)
    !     type is (integer)
    !         print *, "Integer"
    !     type is (real)
    !         print *, "Real"
    !     class is (base_class)
    !         print *, "Base class"
    !     class default
    !         print *, "Unknown"
    ! end select

    ! ==============================================================================
    ! Loops
    ! ==============================================================================

    ! DO loop
    do i = 1, 10
        print *, i
    end do

    ! DO loop with step
    do i = 10, 1, -1
        print *, i
    end do

    ! Named DO loop
    outer: do i = 1, 10
        inner: do j = 1, 10
            if (i * j > 50) exit outer
            if (mod(j, 2) == 0) cycle inner
            print *, i, j
        end do inner
    end do outer

    ! DO WHILE loop
    i = 0
    do while (i < 10)
        i = i + 1
        print *, i
    end do

    ! Infinite loop with EXIT
    do
        i = i + 1
        if (i > 100) exit
    end do

    ! FORALL construct (deprecated, use DO CONCURRENT)
    forall (i = 1:10, j = 1:10, i /= j)
        matrix(i, j) = real(i + j)
    end forall

    ! DO CONCURRENT (Fortran 2008)
    do concurrent (i = 1:10, j = 1:10)
        matrix(i, j) = real(i * j)
    end do

    do concurrent (i = 1:100) local(temp) shared(result)
        temp = compute(i)
        result(i) = temp
    end do

    ! ==============================================================================
    ! Arrays Operations
    ! ==============================================================================

    ! Array assignment
    static_array = 0                    ! All elements = 0
    static_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    static_array = [(i, i=1, 10)]       ! Implied do loop
    static_array = [(i**2, i=1, 10)]    ! Expression in implied do

    ! Array sections
    static_array(1:5) = 0               ! First 5 elements
    static_array(::2) = 1               ! Every other element
    static_array(10:1:-1) = [(i, i=1,10)]  ! Reverse order

    ! Array operations (element-wise)
    static_array = static_array + 1
    static_array = static_array * 2
    matrix = matrix + 1.0

    ! WHERE construct
    where (static_array > 5)
        static_array = 0
    elsewhere (static_array > 0)
        static_array = static_array + 1
    elsewhere
        static_array = -1
    end where

    ! Single-line WHERE
    where (matrix > 0.0) matrix = sqrt(matrix)

    ! Array intrinsic functions
    i = size(static_array)
    i = size(matrix, dim=1)
    print *, shape(matrix)
    print *, lbound(array_bounds)
    print *, ubound(array_bounds)
    print *, maxval(static_array)
    print *, minval(static_array)
    print *, sum(static_array)
    print *, product(static_array)
    flag = any(static_array > 5)
    flag = all(static_array > 0)
    i = count(static_array > 5)

    ! Array reshaping and manipulation
    matrix(1:2, 1:5) = reshape([1,2,3,4,5,6,7,8,9,10], [2, 5])
    ! print *, transpose(matrix)
    ! spread, pack, unpack, merge...

    ! ==============================================================================
    ! Dynamic Memory
    ! ==============================================================================

    ! Allocate
    allocate(dynamic_array(100))
    allocate(alloc_matrix(10, 20))
    allocate(dynamic_string, source="Hello, World!")

    ! Allocate with error handling
    allocate(dynamic_array(1000), stat=i, errmsg=fixed_string)
    if (i /= 0) then
        print *, "Allocation failed: ", trim(fixed_string)
        stop 1
    end if

    ! Deallocate
    if (allocated(dynamic_array)) deallocate(dynamic_array)

    ! Move allocation (Fortran 2003)
    call move_alloc(from=dynamic_array, to=vec)

    ! ==============================================================================
    ! Pointers
    ! ==============================================================================

    ! Pointer assignment
    int_ptr => target_var
    matrix_ptr => target_array  ! This would need proper declaration

    ! Check pointer association
    if (associated(int_ptr)) print *, "Associated"
    if (associated(int_ptr, target_var)) print *, "Associated with target_var"

    ! Nullify pointer
    nullify(int_ptr)
    int_ptr => null()

    ! Allocate through pointer
    allocate(int_ptr)
    int_ptr = 42
    deallocate(int_ptr)

    ! ==============================================================================
    ! Input/Output
    ! ==============================================================================

    ! Print statements
    print *, "Hello, World!"
    print '(A)', "Formatted output"
    print '(I5, F10.3, A20)', i, single_precision, fixed_string

    ! Write statements
    write(*, *) "Hello"
    write(*, '(A)') "Formatted"
    write(output_unit, *) "To standard output"
    write(error_unit, *) "To standard error"

    ! Read statements
    read *, i
    read '(I5)', i
    read(*, *) i, j, k
    read(input_unit, *) single_precision

    ! File I/O
    open(unit=10, file='data.txt', status='new', action='write', iostat=i)
    if (i == 0) then
        write(10, *) "Line 1"
        write(10, *) "Line 2"
        close(10)
    end if

    open(unit=10, file='data.txt', status='old', action='read', iostat=i)
    if (i == 0) then
        read(10, '(A)', iostat=i) fixed_string
        do while (i == 0)
            print *, trim(fixed_string)
            read(10, '(A)', iostat=i) fixed_string
        end do
        close(10)
    end if

    ! INQUIRE statement
    inquire(file='data.txt', exist=flag)
    inquire(unit=10, opened=flag)

    ! Format specifications
    ! I - Integer
    ! F - Fixed-point real
    ! E - Exponential real
    ! G - General real
    ! A - Character
    ! L - Logical
    ! X - Space
    ! / - New line
    ! T - Tab position

    write(*, '(I10)') i                     ! Integer, width 10
    write(*, '(F10.3)') single_precision    ! Real, width 10, 3 decimals
    write(*, '(E15.7)') single_precision    ! Exponential
    write(*, '(ES15.7)') single_precision   ! Scientific
    write(*, '(EN15.7)') single_precision   ! Engineering
    write(*, '(A20)') fixed_string          ! Character, width 20
    write(*, '(L5)') flag                   ! Logical, width 5
    write(*, '(5X, I5)') i                  ! 5 spaces, then integer
    write(*, '(3I5)') i, j, k               ! 3 integers
    write(*, '(10I5)') (static_array(i), i=1,10)  ! Implied do

    ! Internal read/write
    write(fixed_string, '(I5)') i           ! Integer to string
    read(fixed_string, '(I5)') j            ! String to integer

    ! ==============================================================================
    ! String Operations
    ! ==============================================================================

    i = len(fixed_string)                   ! Length
    i = len_trim(fixed_string)              ! Length without trailing spaces
    fixed_string = trim(fixed_string)       ! Remove trailing spaces
    fixed_string = adjustl(fixed_string)    ! Left justify
    fixed_string = adjustr(fixed_string)    ! Right justify
    i = index(fixed_string, "search")       ! Find substring
    i = scan(fixed_string, "abc")           ! Find any character in set
    i = verify(fixed_string, "abc")         ! Find character not in set
    fixed_string = repeat("ab", 5)          ! Repeat string

    ! Character comparisons
    flag = llt("abc", "abd")                ! Lexically less than
    flag = lle("abc", "abc")                ! Less than or equal
    flag = lgt("abd", "abc")                ! Greater than
    flag = lge("abc", "abc")                ! Greater than or equal

    ! Character conversion
    i = ichar('A')                          ! Character to integer
    single_char = char(65)                  ! Integer to character
    i = iachar('A')                         ! ASCII code
    single_char = achar(65)                 ! ASCII to character

    ! ==============================================================================
    ! Intrinsic Procedures
    ! ==============================================================================

    ! Mathematical functions
    single_precision = abs(-3.14)
    single_precision = sqrt(2.0)
    single_precision = exp(1.0)
    single_precision = log(10.0)
    single_precision = log10(100.0)
    single_precision = sin(pi)
    single_precision = cos(pi)
    single_precision = tan(pi/4)
    single_precision = asin(0.5)
    single_precision = acos(0.5)
    single_precision = atan(1.0)
    single_precision = atan2(1.0, 1.0)
    single_precision = sinh(1.0)
    single_precision = cosh(1.0)
    single_precision = tanh(1.0)

    ! Type conversion
    i = int(3.7)
    i = nint(3.5)                           ! Nearest integer
    single_precision = real(i)
    float64 = dble(i)
    complex_single = cmplx(1.0, 2.0)

    ! Complex operations
    single_precision = real(complex_single)
    single_precision = aimag(complex_single)
    complex_single = conjg(complex_single)

    ! Bit operations
    i = iand(5, 3)                          ! Bitwise AND
    i = ior(5, 3)                           ! Bitwise OR
    i = ieor(5, 3)                          ! Bitwise XOR
    i = not(5)                              ! Bitwise NOT
    i = ibset(0, 3)                         ! Set bit
    i = ibclr(15, 3)                        ! Clear bit
    flag = btest(8, 3)                      ! Test bit
    i = ishft(8, 2)                         ! Shift
    i = ishftc(8, 2)                        ! Circular shift

    ! Random numbers
    call random_seed()
    call random_number(single_precision)
    call random_number(matrix)

    ! System procedures
    call system_clock(count=i, count_rate=j, count_max=k)
    call cpu_time(single_precision)
    call date_and_time(date=fixed_string)
    ! call execute_command_line("ls")       ! Fortran 2008
    ! call get_environment_variable("HOME", fixed_string)

    ! ==============================================================================
    ! Error Handling
    ! ==============================================================================

    ! Stop execution
    ! stop                                  ! Normal termination
    ! stop 1                                ! With exit code
    ! stop "Error message"                  ! With message
    ! error stop                            ! Error termination (Fortran 2008)
    ! error stop 1
    ! error stop "Error!"

    contains

    ! ==============================================================================
    ! Internal Procedures
    ! ==============================================================================

    subroutine internal_sub(x, y)
        real, intent(in) :: x
        real, intent(out) :: y
        y = x * 2.0
    end subroutine internal_sub

    function internal_func(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x ** 2
    end function internal_func

end program sample_fortran

! ==============================================================================
! Module Definition
! ==============================================================================

module my_module
    use, intrinsic :: iso_fortran_env
    implicit none

    private  ! Default private
    public :: my_type, my_subroutine, my_function, my_interface
    public :: assignment(=), operator(+)

    ! Module constants
    real(real64), parameter :: module_pi = 3.14159265358979_real64

    ! Module variables (use sparingly)
    integer, save :: module_counter = 0

    ! Abstract interface
    abstract interface
        function my_interface(x) result(y)
            import :: real64
            real(real64), intent(in) :: x
            real(real64) :: y
        end function my_interface
    end interface

    ! Derived type with OOP features
    type :: my_type
        private
        real :: x = 0.0
        real :: y = 0.0
    contains
        procedure :: init => my_type_init
        procedure :: display => my_type_display
        procedure :: get_x
        procedure :: set_x
        procedure, private :: helper_method
        generic :: assignment(=) => assign_from_array
        final :: my_type_finalize
    end type my_type

    ! Extended type (inheritance)
    type, extends(my_type) :: extended_type
        real :: z = 0.0
    contains
        procedure :: display => extended_display  ! Override
    end type extended_type

    ! Abstract type
    type, abstract :: abstract_base
    contains
        procedure(abstract_method), deferred :: abstract_method
    end type abstract_base

    ! Operator interface
    interface operator(+)
        module procedure add_my_types
    end interface

    interface assignment(=)
        module procedure assign_from_array
    end interface

    ! Generic interface
    interface generic_func
        module procedure generic_int
        module procedure generic_real
    end interface

contains

    subroutine my_subroutine(a, b, c, d, status)
        ! Intent specifications
        integer, intent(in) :: a              ! Input only
        integer, intent(out) :: b             ! Output only
        integer, intent(inout) :: c           ! Both
        integer, intent(in), optional :: d    ! Optional
        integer, intent(out), optional :: status

        ! Value attribute (Fortran 2003)
        ! integer, value :: local_copy

        ! Local variables
        integer :: local_var

        b = a * 2
        c = c + a

        if (present(d)) then
            b = b + d
        end if

        if (present(status)) status = 0

    end subroutine my_subroutine

    function my_function(x) result(y)
        real, intent(in) :: x
        real :: y

        y = x ** 2 + 2*x + 1
    end function my_function

    ! Pure function (no side effects)
    pure function pure_func(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * 2
    end function pure_func

    ! Elemental function (works on arrays element-wise)
    elemental function elemental_func(x) result(y)
        real, intent(in) :: x
        real :: y
        y = sin(x) + cos(x)
    end function elemental_func

    ! Recursive function
    recursive function factorial(n) result(f)
        integer, intent(in) :: n
        integer :: f

        if (n <= 1) then
            f = 1
        else
            f = n * factorial(n - 1)
        end if
    end function factorial

    ! Type-bound procedures
    subroutine my_type_init(this, x, y)
        class(my_type), intent(inout) :: this
        real, intent(in) :: x, y
        this%x = x
        this%y = y
    end subroutine my_type_init

    subroutine my_type_display(this)
        class(my_type), intent(in) :: this
        print *, "x =", this%x, "y =", this%y
    end subroutine my_type_display

    pure function get_x(this) result(x)
        class(my_type), intent(in) :: this
        real :: x
        x = this%x
    end function get_x

    subroutine set_x(this, x)
        class(my_type), intent(inout) :: this
        real, intent(in) :: x
        this%x = x
    end subroutine set_x

    subroutine helper_method(this)
        class(my_type), intent(in) :: this
        ! Private helper
    end subroutine helper_method

    subroutine extended_display(this)
        class(extended_type), intent(in) :: this
        print *, "x =", this%x, "y =", this%y, "z =", this%z
    end subroutine extended_display

    subroutine my_type_finalize(this)
        type(my_type), intent(inout) :: this
        ! Cleanup code
    end subroutine my_type_finalize

    function add_my_types(a, b) result(c)
        type(my_type), intent(in) :: a, b
        type(my_type) :: c
        c%x = a%x + b%x
        c%y = a%y + b%y
    end function add_my_types

    subroutine assign_from_array(this, arr)
        class(my_type), intent(out) :: this
        real, intent(in) :: arr(2)
        this%x = arr(1)
        this%y = arr(2)
    end subroutine assign_from_array

    function generic_int(x) result(y)
        integer, intent(in) :: x
        integer :: y
        y = x * 2
    end function generic_int

    function generic_real(x) result(y)
        real, intent(in) :: x
        real :: y
        y = x * 2.0
    end function generic_real

    subroutine abstract_method(this)
        import :: abstract_base
        class(abstract_base), intent(in) :: this
    end subroutine abstract_method

end module my_module

! ==============================================================================
! Submodule (Fortran 2008)
! ==============================================================================

submodule (my_module) my_submodule
    implicit none

contains

    ! Implementation of module procedures
    module subroutine submodule_proc()
        ! Implementation
    end subroutine submodule_proc

end submodule my_submodule

! ==============================================================================
! Block Data (Legacy Feature)
! ==============================================================================

block data initial_values
    implicit none
    common /block1/ x, y, z
    real :: x, y, z
    data x, y, z /1.0, 2.0, 3.0/
end block data initial_values

! ==============================================================================
! End of Fortran Sample
! ==============================================================================
