 subroutine error3()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
!                 ^^ f90-ts-font-lock-bracket-face

 contains
!^^^^^^^^ font-lock-keyword-face

    ! comment
!   ^^^^^^^^^ font-lock-comment-face

    sfunction f()
!   ^ (f90-ts-font-lock-error-face)
!    ^^^^^^^^ font-lock-keyword-face
!             ^ font-lock-function-name-face
!              ^^ f90-ts-font-lock-bracket-face
     end function f
!    ^^^ font-lock-keyword-face
!        ^^^^^^^^ font-lock-keyword-face
!                 ^ font-lock-function-name-face

 end subroutine error3
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face
