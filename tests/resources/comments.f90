submodule (mod) subm

 ! comment 1

 use other

 ! comment 2
 ! comment 3
 implicit none

 ! before contains1
 ! before contains2

contains

! ==========
! ==========

   ! after contains1
 ! after contains2

 subroutine sub1()
 ! arguments
 ! ==========
      ! comment sub11

      ! comment sub12
 contains

    ! comment fun11
    ! comment fun12
 ! ==========
 ! ==========

    function fun1()
    ! ==========
    ! ==========
         ! comment fun1_inside1
         ! comment fun1_inside2
    end function fun1
 end subroutine sub1


 ! contains in subroutine 1
 ! contains in subroutine 2

 pure function fun2()
      ! comment fun21

      ! comment fun22
 end function fun2

end submodule subm
