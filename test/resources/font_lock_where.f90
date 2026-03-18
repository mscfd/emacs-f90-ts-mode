 subroutine sub()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^ font-lock-function-name-face
!              ^^ f90-ts-font-lock-bracket-face
      log_a: where (a > 0.0)
!     ^^^^^ nil
!          ^ f90-ts-font-lock-delimiter-face
!            ^^^^^ font-lock-keyword-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^ nil
!                     ^ f90-ts-font-lock-operator-face
!                       ^^^ font-lock-number-face
!                          ^ f90-ts-font-lock-bracket-face
           b = log(a)
!          ^ nil
!            ^ f90-ts-font-lock-operator-face
!              ^^^ font-lock-builtin-face
!                 ^ f90-ts-font-lock-bracket-face
!                  ^ nil
!                   ^ f90-ts-font-lock-bracket-face
      elsewhere (a == 0.0) log_a
!     ^^^^^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ nil
!                  ^^ f90-ts-font-lock-operator-face
!                     ^^^ font-lock-number-face
!                        ^ f90-ts-font-lock-bracket-face
!                          ^^^^^ nil
           b = 0.0
!          ^ nil
!            ^ f90-ts-font-lock-operator-face
!              ^^^ font-lock-number-face
           is_zero = .true.
!          ^^^^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^^^^^^ font-lock-constant-face
      elsewhere log_a
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^ nil
           ! negative
!          ^^^^^^^^^^ font-lock-comment-face
           b = -1.0
!          ^ nil
!            ^ f90-ts-font-lock-operator-face
!              ^ nil
!               ^^^ font-lock-number-face
           is_negative = .true.
!          ^^^^^^^^^^^ nil
!                      ^ f90-ts-font-lock-operator-face
!                        ^^^^^^ font-lock-constant-face
      end where log_a
!     ^^^ font-lock-keyword-face
!         ^^^^^ font-lock-keyword-face
!               ^^^^^ nil
 end subroutine sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^ font-lock-function-name-face
