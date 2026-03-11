subroutine sub()
     ! list context alignment of numbers in array
     integer, dimension(1:3,1:4), parameter:: &
            facet_ix = reshape([2,3,4, &
                               1,4,3, &
                               1,2,4, &
                               1,3,2], [3,4])
end subroutine sub
