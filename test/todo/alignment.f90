subroutine sub()
     ! list context alignment of numbers in array
     integer, dimension(1:3,1:4), parameter:: &
            facet_ix = reshape([2,3,4, &
                               1,4,3, &
                               1,2,4, &
                               1,3,2], [3,4])
end subroutine sub


! list context for case range
subroutine select_case()
     select_variant: select case (variant)
     case ('version1')
          print *, 'version1'
     case ('version2', &
            'extra')
          call xyz()
     case default
          call uvw()
     end select select_variant
end subroutine select_case
