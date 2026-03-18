! import is not highlighted
abstract interface
     subroutine sub_ifc(x)
          import some_t
          class(some_t), intent(inout) :: x
     end subroutine sub_ifc
end interface


! none and external not highlighted
program program3
 implicit none (type, external)
 integer :: i
 do i = 1,10
      call sub(i)
 end do
contains
 subroutine sub(k)
      integer, intent(in) :: k
      print *,k
 end subroutine sub
end program program3
