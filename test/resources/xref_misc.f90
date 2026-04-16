program xref_test
 implicit none
 integer :: a, b, result

 call init_values(a, b)
!@ref init_values#5,17,22
!@def init_values#17
 result = add_values(a, b)
!@def add_values#25
!@ref add_values#8,25,31
!@ref result#3,8,12 a#3,5,8 b#3,5,8
 print *, "Result = ", result
!@ref result#3,8,12

contains

 subroutine init_values(x, y)
!@def init_values#17
      integer, intent(out) :: x, y
      x = 10
      y = 20
 end subroutine init_values


 function add_values(x, y) result(sum)
!@def add_values#25
      integer, intent(in) :: x, y
      integer :: sum
      sum = x + y
!@ref sum#25,28,29
 end function add_values
end program xref_test

module math_utils
!@def math_utils#34

contains
  subroutine multiply_values(x, y, prod)
!@def multiply_values#38
    integer, intent(in) :: x, y
    integer, intent(out) :: prod
    prod = x * y
!@ref prod#38,41,42
  end subroutine multiply_values
!@def multiply_values#38
end module math_utils
!@def math_utils#34
