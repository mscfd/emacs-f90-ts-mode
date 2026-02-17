 submodule (mod) subm
!^^^^^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^ font-lock-function-name-face
!              ^ f90-ts-font-lock-bracket-face
!                ^^^^ font-lock-function-name-face
  ! comment
! ^^^^^^^^^ font-lock-comment-face
 contains
!^^^^^^^^ font-lock-keyword-face
 ! ==========
!^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
  ! after seperator
! ^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine sub()
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^ font-lock-function-name-face
!               ^^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       ! comment sub
!      ^^^^^^^^^^^^^ font-lock-comment-face
  contains
! ^^^^^^^^ font-lock-keyword-face
       ! comment code
!      ^^^^^^^^^^^^^^ font-lock-comment-face
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
  end subroutine sub
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^ font-lock-function-name-face
 end submodule subm
!^^^ font-lock-keyword-face
!    ^^^^^^^^^ font-lock-keyword-face
!              ^^^^ font-lock-function-name-face
