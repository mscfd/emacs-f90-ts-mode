 subroutine error2()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
!                 ^^ f90-ts-font-lock-bracket-face
      associate(x => y)
!     ^^^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
      end asociate
!     ^^^ font-lock-keyword-face
!         ^^^^^^^^ f90-ts-font-lock-error-face
 end subroutine error2
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face
