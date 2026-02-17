 function fun(self, other_self) result(this_result)
!^^^^^^^^ font-lock-keyword-face
!         ^^^ font-lock-function-name-face
!            ^ f90-ts-font-lock-bracket-face
!             ^^^^ f90-ts-font-lock-special-var-face
!                 ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                               ^^^^^^ font-lock-keyword-face
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^^^^^^^^^^^ default
!                                                 ^ f90-ts-font-lock-bracket-face
      logical :: this_result
!     ^^^^^^^ font-lock-type-face
!             ^^ f90-ts-font-lock-delimiter-face
!                ^^^^^^^^^^^ nil
      class(tMyself), intent(in) :: self
!     ^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^ font-lock-type-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^^^^^ font-lock-keyword-face
!                                ^^ f90-ts-font-lock-delimiter-face
!                                   ^^^^ f90-ts-font-lock-special-var-face
      class(tThisType), intent(out) :: other_self
!     ^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^^^ font-lock-type-face
!                    ^ f90-ts-font-lock-bracket-face
!                     ^ f90-ts-font-lock-delimiter-face
!                       ^^^^^^^^^^^ font-lock-keyword-face
!                                   ^^ f90-ts-font-lock-delimiter-face
!                                      ^^^^^^^^^^ nil

      call copy(self, other_self)
!     ^^^^ font-lock-keyword-face
!          ^^^^ font-lock-function-name-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ f90-ts-font-lock-special-var-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^^^^^ nil
!                               ^ f90-ts-font-lock-bracket-face
      this_result = other_self%check()
!     ^^^^^^^^^^^ nil
!                 ^ f90-ts-font-lock-operator-face
!                   ^^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-operator-face
!                              ^^^^^ nil
!                                   ^^ f90-ts-font-lock-bracket-face
 end function fun
!^^^ font-lock-keyword-face
!    ^^^^^^^^ font-lock-keyword-face
!             ^^^ font-lock-function-name-face

 subroutine sub(this, x)
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^ font-lock-function-name-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ f90-ts-font-lock-special-var-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^ nil
!                      ^ f90-ts-font-lock-bracket-face
      class(yourself_t), intent(inout) :: this
!     ^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^^^^ font-lock-type-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^^^^^^^^^ font-lock-keyword-face
!                                      ^^ f90-ts-font-lock-delimiter-face
!                                         ^^^^ f90-ts-font-lock-special-var-face
      integer, intent(in) :: x
!     ^^^^^^^ font-lock-type-face
!            ^ f90-ts-font-lock-delimiter-face
!              ^^^^^^^^^^ font-lock-keyword-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^ nil
      call self%set_val(x)
!     ^^^^ font-lock-keyword-face
!          ^^^^ f90-ts-font-lock-special-var-face
!              ^ f90-ts-font-lock-operator-face
!               ^^^^^^^ nil
!                      ^ f90-ts-font-lock-bracket-face
!                       ^ nil
!                        ^ f90-ts-font-lock-bracket-face
 end subroutine sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^ font-lock-function-name-face
