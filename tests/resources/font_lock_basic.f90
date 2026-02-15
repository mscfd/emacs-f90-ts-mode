 program prog1
!^^^^^^^ font-lock-keyword-face
!        ^^^^^ font-lock-function-name-face
  integer :: i
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^ nil
  ! comment
! ^^^^^^^^^ font-lock-comment-face
  i = 65
! ^ nil
!   ^ f90-ts-font-lock-operator-face
!     ^^ font-lock-number-face
 end program prog1
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^ font-lock-function-name-face
