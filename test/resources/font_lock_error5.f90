 subroutine sub()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^ font-lock-function-name-face
!              ^^ f90-ts-font-lock-bracket-face

      associate(
!     ^^^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
           x = 1
!          ^^ (f90-ts-font-lock-error-face)
!            ^ (f90-ts-font-lock-operator-face f90-ts-font-lock-error-face)
!             ^^ (f90-ts-font-lock-error-face)
           x = 2   
!^^^^^^^^^^^^ (f90-ts-font-lock-error-face)
!            ^ (f90-ts-font-lock-operator-face f90-ts-font-lock-error-face)
!             ^^ (f90-ts-font-lock-error-face)
           x = 3    
!^^^^^^^^^^^^ (f90-ts-font-lock-error-face)
!            ^ (f90-ts-font-lock-operator-face f90-ts-font-lock-error-face)
!             ^^ (f90-ts-font-lock-error-face)
           x = 4
!            ^ (f90-ts-font-lock-operator-face)
           x = 5
!            ^ (f90-ts-font-lock-operator-face)
           x = 6
!            ^ (f90-ts-font-lock-operator-face)

 end subroutine sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^ nil
