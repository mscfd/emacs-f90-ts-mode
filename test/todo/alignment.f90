subroutine sub()
     ! list context alignment of numbers in array
     integer, dimension(1:3,1:4), parameter:: &
            facet_ix = reshape([2,3,4, &
                               1,4,3, &
                               1,2,4, &
                               1,3,2], [3,4])
end subroutine sub

! y is not aligned under 5 with 'keep-or-primary in indent-region operation
! reason: primary is the node with the smallest column number, hence 7 or 8,
! as the old indentation is accessed
! solution: much like the existing cache, initialise a vector and store already
! computed offset for each line in a multiline statement
! note: this example is already in indent_region_align_expr.erts, with the wrong indentation
subroutine f()
x = 5 + 6 &
* 7 &
- 8 + &
y
end subroutine f



! line at closing ")" is not indented according to
! offset f90-ts-mode-indent-paren-close
! reason: handle parenthesis (in argument_list) by other with offset,
! filter from regular items (which always have zero offset)
subroutine args()
     x = my_fun(&
                arg1, &
                arg2 &
               )
end subroutine args
