module ifc

type, abstract :: base
!@def base#3
!@ref base#3,6,8,13,25,29
end type base

type, extends(base) :: impl
!@def impl#8
!@ref impl#8,18,28,31
     procedure(test), pointer :: other_sub => null()
!@ref test#11,16,22,37 other_sub#11,33
     class(base), pointer :: p => null()
!@ref base#3,6,8,13,25,29
contains
     procedure :: test
!@def test#22
end type impl

contains

 function test(self) result(more)
!@def test#22
!@ref self#22,28,33
      class(base) :: more
!@def base#3
!@ref base#3,6,8,13,25,29 more#22,25,35
      class(impl), intent(in) :: self
      class(base), pointer :: bar
!@ref bar#29,31,33,35
      allocate(impl :: bar)
!@ref impl#8,18,28,31
      call self%other_sub(bar)
!@ref other_sub#11,33
      more => bar
!@ref more#22,25,35 bar#29,31,33,35
 end subroutine test

end module ifc
