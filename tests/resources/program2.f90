program prog2

 use some_module

 implicit none

 type, extends(some_type) :: extra_stuff
      integer :: ival = 123
 contains
      procedure :: init
      procedure :: dispose
 end type extra_stuff

 class(extra_stuff), pointer :: p

 allocate(extra_stuff :: p)
 call p%init(456)
 print *, p%ival
 call p%dispose()

contains

 subroutine init(self, i)
      class(extra_type), intent(out) :: self
      integer, intent(in) :: i
      self%ival = i
 end subroutine init

 subroutine dispose(self)
      class(extra_type), intent(inout) :: self
      self = extra_type()
 end subroutine dispose

end program prog2
