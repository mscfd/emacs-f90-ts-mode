subroutine sub()
     ! list context alignment of numbers in array
     integer, dimension(1:3,1:4), parameter:: &
            facet_ix = reshape([2,3,4, &
                               1,4,3, &
                               1,2,4, &
                               1,3,2], [3,4])
end subroutine sub


subroutine sub()
     ! always add first position after '(' to selection
     ! do not align .or. with other?
     if (     condition1(x) &
         .or. condition2(x)) then
     end if
end subroutine sub


! question: to which column should comments align,
! looks like almost all currently?
subroutine sub()
     associate(test => 1+2, &
               ! comment
               after_comment_line => 3+4)
     end associate

     associate(test => 1+2, &

               ! comment

               after_empty_and_comment_lines => 3+4)
     end associate
end subroutine sub
