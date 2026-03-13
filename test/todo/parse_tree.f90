! indent last line only, or do indent-region (which originally
! started smart end completion at last line)
! problem: parse tree is broken at this point (can/should we do anything about that)
module do_mod
contains
 subroutine do_sub()
 end function do_fun
end module do_mod


! executing f90-ts-enlarge-region executed at |
! shows that the NEWLINE is part of the type node
module mod_init_4()
|   type, public :: t
      integer :: i
   end type t
end module mod_init_4


! the if line itself does not have a named node for itself,
! but the end line has, it bit strange when experimenting with mark region operations
subroutine sub_init_3()
   if (cond(x,y)) then
      call other()
   end if
end subroutine sub_init_3
