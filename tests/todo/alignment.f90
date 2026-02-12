subroutine sub()
   ! list context alignment of numbers in array
   _integer4fpm_, dimension(1:3,1:4), parameter:: &
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
