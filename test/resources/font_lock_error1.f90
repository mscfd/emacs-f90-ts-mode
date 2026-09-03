 subroutine error1()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
!                 ^^ f90-ts-font-lock-bracket-face
      x: if (cond) then
!     ^ nil
!      ^ f90-ts-font-lock-delimiter-face
!        ^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^ nil
!                ^ f90-ts-font-lock-bracket-face
!                  ^^^^ font-lock-keyword-face
      end i x
!     ^^^ font-lock-keyword-face
!         ^^^ f90-ts-font-lock-error-face
 end subroutine error1
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face
