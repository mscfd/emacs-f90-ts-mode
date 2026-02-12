! indent last line only, or do indent-region (which originally
! started smart end completion at last line)
! problem: parse tree is broken at this point (can/should we do anything about that)
module do_mod
contains
 subroutine do_sub()
 end function do_fun
end module do_mod
