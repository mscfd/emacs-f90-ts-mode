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


! alignment and fontlocking issues
subroutine sub()
     map_a: where (a > 0.0)
     b = sqrt(a)
     c = log(a)
     elsewhere (a == 0.0) map_a
     b = 0.0
     c = 0.0
     elsewhere map_a
     b = -1.0
     c = -1.0
     end where map_a
end subroutine sub



subroutine sub()
     forall (i = 1:n, j = 1:n, a(i,j) /= 0.0)
     b(i,j) = 1.0 / a(i,j)
     c(i,j) = b(i,j) * 2.0
     end forall

     forall (i = 1:n)  a(i,i) = 1.0
end subroutine sub
