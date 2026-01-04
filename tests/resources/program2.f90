program prog2

 implicit none

 type, abstract :: base_t
      integer :: ival = 123
 end type base_t

 type, extends(base_t) :: extra_t
      integer :: jval = 456
 end type extra_t

 class(base_t), pointer :: p

 allocate(extra_t :: p)
 select type (p)
 class is (extra_t)
      call p%set(i=456, &
                 j=123)
      print *, p%ival
      call p%unset()
 class default
      print *, 'unknown type'
 end select

contains

 subroutine set(obj, i, j)
      class(extra_t), intent(out) :: obj
      integer, intent(in) :: i,&
                             j
      obj%ival = i
      obj%jval = j
 end subroutine set

 subroutine unset(obj)
      class(extra_t), intent(inout) :: obj
      call obj%set(i=123, &
                   j=456)
 end subroutine unset

end program prog2
