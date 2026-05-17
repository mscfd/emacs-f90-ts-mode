! comment within a continued string is not yet supported by tree-sitter
subroutine sub()
   str = "012&
         ! comment
         &abc &
         &uvw"
end subroutine sub


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
module mod
   type, public :: t
      integer :: i
   end type t
end module mod


! the if line itself does not have a named node for itself,
! but the "end if" line has, it is bit strange when experimenting
! with mark region operations:
! executing enlarge region with point at if: whole statement
! executing enlarge region with point at end if: just the line
subroutine sub()
   if (cond(x,y)) then
      call other()
   end if
end subroutine sub

! tree for the assignment line:
! (assignment_statement left: (identifier) =
!   right: (math_expression left: (number_literal) operator: + & & right: (number_literal)))
! should we rather generate four ampersand & & & &, two of which are virtual?
! that would match the tree with leading ampersands and would be more consistent,
! this would also help with correct indentation of the line containing 5 (which has no leading ampersand)
! note: this is not valid code, but might easily occur during typing
subroutine sub()
   x123456 = 6 + &
            &
             5
end subroutine sub
