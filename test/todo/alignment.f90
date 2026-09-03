! use commata as anchor, maybe like:
! if node is not a commata and directly followed by a commata, then a previous commata
! is a good anchor with offset -lenght of node?
subroutine array
   use, intrinsic :: iso_fortran_env, only : real64
   integer, parameter :: dp = real64
   real(kind=dp), parameter :: g = (1.0 + sqrt(5.0)) / 2.0

   real(kind=dp), dimension(1:3,1:4), parameter :: &
          arr = reshape( &
          [      g,      1.0_dp,     1.0_dp, &
                -g,      2.0_dp,     2.0_dp, &
            1.0_dp,           g,         -g, &
           -1.0_dp,          -g,          g], &
          [3,4])
end subroutine array


! there is no proper context node for "! comment 2", but "! comment 1"
! would be a proper primary or secondary choice, besides default indentation shown below
! in both cases: add assignment_statement as list context and extract part after "="
subroutine math()
     integer, parameter :: x = 5 + 6
     x1234567890 = & ! comment 1
            ! comment 2
            5 + 6
end subroutine math

! y is not a math-expression, just an identifier,
! should identifier be added?
! (in order to get column after = as suggestion,
! especially if a comment is present, this makes sense)
subroutine math()
     x1234567890 = &
            (x+y)
     x1234567890 = & ! commen
            (x+y)
     z = &
            y
end subroutine math



! we need to include init_declarator context to indent
! "[ & ..." after "=" relative to "rgb =" ...
subroutine arr3()
     character(len=5), dimension(1:3) :: & ! comment
                                         ! comment
        &   rgb = &
        &   [ &
        &    'red  ', &
        &    'green', &
        &    'blue ' ], &
        &   alpha = 'trnsp'
end subroutine arr3


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


! align "result" below predicate_ifc?
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
