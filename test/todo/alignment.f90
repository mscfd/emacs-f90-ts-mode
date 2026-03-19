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

! align use-only statements
module foo
 use mod1
 use mod2, only: sub1, &
        sub2
 use mod3, &
        mod4
 ! another
 mod5, only: baz1,&
           baz2

contains
 subroutine sub_bar()
      use mod3, only: fun1 => mod3_fun1, &
             fun2 => mod3_fun2, &
             ! one more
             fun3 => mod3_fun3
 end subroutine sub_bar
end module foo


! align public items statements
module foo
 public sub1, sub2
 public :: sub3, &
        sub4, &
        ! comment
        ! comment
        sub5
 public    fun1, &
        fun2, &
        ! comment
        ! comment
        fun3
end module foo

! align private items statements
module foo
 private sub1, sub2
 private :: sub3, &
        sub4, &
        ! comment
        ! comment
        sub5
 private    fun1, &
        fun2, &
        ! comment
        ! comment
        fun3
end module foo
