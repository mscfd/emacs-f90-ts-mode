module ifc
!@def ifc#1
!@ref ifc#1,58

 interface ifc_gen
!@def ifc_gen#5
!@ref ifc_gen#5,10,49
      module procedure ifc_gen_impl
!@ref ifc_gen_impl#8
 end interface ifc_gen

 interface operator(.ifctwo.)
!@def .ifctwo.#12
!@ref .ifctwo.#12,16,52
      module procedure ifctwo_impl
 end interface operator(.ifctwo.)

 interface operator(+)
!@def +#18
!@ref +#18,22
      module procedure plus
 end interface operator(+)

 interface assignment(=)
!@def =#24
      module procedure assign_impl
 end interface assignment(=)

 type :: some_t
 end type some_t

 abstract interface
      import some_t
      subroutine sub_ifc(t)
!@def sub_ifc#34
!@ref sub_ifc#34,40,46
           class(some_t), intent(inout) :: t
!@def some_t#29
!@ref some_t#29,30,33,37 t#34,37
      end subroutine sub_ifc
 end interface

contains

 subroutine sub(some_bar)
      procedure(sub_ifc) :: some_bar
!@def sub_ifc#34
!@ref sub_ifc#34,40,46 some_bar#45,46,49
      call ifc_gen(some_bar)
!@def ifc_gen#5
!@ref ifc_gen#5,10,49
      x = y .ifctwo. z
!@def .ifctwo.#12
!@ref .ifctwo.#12,16,52
      call assign_impl(x, y)
!@ref assign_impl#26,55
 end subroutine sub
end module ifc
