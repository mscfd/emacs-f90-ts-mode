 subroutine values()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
!                 ^^ f90-ts-font-lock-bracket-face
      x = 5
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^ font-lock-number-face
      z = (6.0, 5.0) + (2e4_dp, -3e2_dp)
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^^^^^^^^^ font-lock-number-face
!                    ^ f90-ts-font-lock-operator-face
!                      ^^^^^^^^^^^^^^^^^ font-lock-number-face
      s = "abc"
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^^^^ font-lock-string-face
      l = .true. .or. .false.
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^^^^^ font-lock-constant-face
!                ^^^^ f90-ts-font-lock-operator-face
!                     ^^^^^^^ font-lock-constant-face
      p => null()
!     ^ nil
!       ^^ f90-ts-font-lock-delimiter-face
!          ^^^^^^ font-lock-constant-face
 1234 stmt_label='here'
!^^^^ font-lock-constant-face
!     ^^^^^^^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^ font-lock-string-face
 end subroutine values
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face

 #define x
!^^^^^^^ font-lock-preprocessor-face
!        ^ font-lock-constant-face
 #ifdef x
!^^^^^^ font-lock-preprocessor-face
!       ^ font-lock-constant-face
 #elifdef x
!^^^^^^^^ font-lock-preprocessor-face
!         ^ font-lock-constant-face
 #endif
!^^^^^^ font-lock-preprocessor-face

 #ifndef x
!^^^^^^^ font-lock-preprocessor-face
!        ^ font-lock-constant-face
 #endif
!^^^^^^ font-lock-preprocessor-face
