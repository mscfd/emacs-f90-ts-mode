! parenthesized_expression needs to be included in the list-context handling
! "commment 2" is child of parenthesized_expression, and thus not recognised
! as part of the following math_expression
subroutine math()
     x1234567890 = ( & ! comment 1
            ! comment 2
            5 + 6 &
                      )
end subroutine math

! primary column for facet_ix is after "::", if there is no relevant item
! on the previous line, maybe the standard offset is more appropriate
! (same for argument lists etc.)
subroutine arr1()
     ! list context alignment of numbers in array
     integer, dimension(1:3,1:4), parameter:: &
        &                                     facet_ix = reshape(&
        &                                                        [2,3,4, &
        &                                                         1,4,3, &
        &                                                         1,2,4, &
        &                                                         1,3,2 &
        &                                                           ], [3,4])
end subroutine arr1


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


! read/write lists
subroutine sub()
     write(*,format_string_for_testing) val1, &       !comment
            ! comment
            val2, val3, &
                  val4, & ! comment

            ! some more comment


            ! comment

            final_val
end subroutine sub



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


! alignment in loop part
function outer(a, b, n) result(T)
     integer, intent(in) :: n
     real, intent(in)    :: a(n), b(n)
     real                :: T(n, n)
     integer             :: i, j

     prod: do concurrent (i = 1:n,&
            j = 1:n)
          T(i, j) = a(i) * b(j)
     end do prod
end function outer


! alignment within reduce, shared, local, local_init
program int23
 integrate: do, concurrent (i = 1:n) default(none) &
        shared(x, y, z, scale) &
        local(tmp) &
        local_init(test) &
        reduce(+: int2, &
        int3)
      tmp  = (real(i)-0.5) * scale
      x(i) = tmp
      y(i) = tmp**2
      z(i) = tmp**3
      int2 = int2 + y(i)
      int3 = int3 + z(i)
 end do integrate
end program int23


! alignment of import statement
module mod
 abstract interface
      subroutine sub_ifc(x, y, z)
           import some_t, some_s, &
                other_r
           class(some_t), intent(inout) :: x
           type(some_s), intent(in) :: y
           class(other_r), pointer, intent(out) :: z
      end subroutine sub_ifc
 end interface
end module mod



! align result below predicate_ifc?
module mod
 abstract interface
      impure function predicate_ifc(self, k1, k2, &
                                          x1, x2) &
             result(flag)
           import some_t, other_t, &
                  key_t
           logical :: flag
           class(some_t), intent(in) :: self
           class(key_t),  intent(in) :: k1, &
                                        k2
           class(*),      intent(in) :: x1
           type(other_t), intent(in) :: x2
      end function predicate_ifc
 end interface
end module mod
