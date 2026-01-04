program program1

 use, intrinsic :: iso_fortran_env

 abstract interface
      real(real32) function f_ifc(x)
           import real32
           real(real32), intent(in) :: x
      end function f_ifc
 end interface

 real(real32) :: s
 integer, parameter :: N1 = 1, &
                       N2 = 100

 s = compute_sequence_sum(reciprocal, N1, N2)
 call print_result(s)

contains

 function compute_sequence_sum(fun, N1, N2) result(s_fun)
 ! result
      real(real32) :: s_fun
 ! arguments
      procedure(f_ifc) :: fun
      integer, intent(in) :: N1, N2
 ! ==========
      s_fun = 0.0
      do i = N1, N2
           block
                real(real32) :: r
                r = real(i, real32)
                s_fun = s_fun + fun(r)
           end block
      end do
 end function compute_sequence_sum

 real(real32) function reciprocal(x)
 ! arguments
      real(real32), intent(in) :: x
 ! ==========
      reciprocal = 1.0_real32 / x
 end function reciprocal

 subroutine print_result(s)
      real(real32), intent(in) :: s
      print '(a,f15.5)', 'result: ', s
 end subroutine print_result

end program program1
