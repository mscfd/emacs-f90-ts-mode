 subroutine op_logical()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-function-name-face
!                     ^^ f90-ts-font-lock-bracket-face
      flag = .not. .true.
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^^^^^ f90-ts-font-lock-operator-face
!                  ^^^^^^ font-lock-constant-face
      flag = .myop. flag
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^^^^^^ f90-ts-font-lock-operator-face
!                   ^^^^ nil
      flag = a .and. (b .or. c)
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^ nil
!              ^^^^^ f90-ts-font-lock-operator-face
!                    ^ f90-ts-font-lock-bracket-face
!                     ^ nil
!                       ^^^^ f90-ts-font-lock-operator-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
      flag = (f1 .eqv. f2) .or. (f1 .neqv. f2)
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^ f90-ts-font-lock-bracket-face
!             ^^ nil
!                ^^^^^ f90-ts-font-lock-operator-face
!                      ^^ nil
!                        ^ f90-ts-font-lock-bracket-face
!                          ^^^^ f90-ts-font-lock-operator-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^^ nil
!                                   ^^^^^^ f90-ts-font-lock-operator-face
!                                          ^^ nil
!                                            ^ f90-ts-font-lock-bracket-face
 end subroutine op_logical
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^ font-lock-function-name-face

 subroutine op_math()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^ font-lock-function-name-face
!                  ^^ f90-ts-font-lock-bracket-face
      x = 65 + (-75)
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^ font-lock-number-face
!            ^ f90-ts-font-lock-operator-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                ^^ font-lock-number-face
!                  ^ f90-ts-font-lock-bracket-face
      y = fun1() .custom. (fun2() .custom. x)
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^^^ nil
!             ^^ f90-ts-font-lock-bracket-face
!                ^^^^^^^^ f90-ts-font-lock-operator-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^^^^ nil
!                              ^^ f90-ts-font-lock-bracket-face
!                                 ^^^^^^^^ f90-ts-font-lock-operator-face
!                                          ^ nil
!                                           ^ f90-ts-font-lock-bracket-face
      z = 1 + 2 * 3 - 4 / 5 ** 6
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^ font-lock-number-face
!           ^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!               ^ f90-ts-font-lock-operator-face
!                 ^ font-lock-number-face
!                   ^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
!                       ^ f90-ts-font-lock-operator-face
!                         ^ font-lock-number-face
!                           ^^ f90-ts-font-lock-operator-face
!                              ^ font-lock-number-face
      flag = .not. cond1 .imply. cond2
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^^^^^ f90-ts-font-lock-operator-face
!                  ^^^^^ nil
!                        ^^^^^^^ f90-ts-font-lock-operator-face
!                                ^^^^^ nil
 end subroutine op_math
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^ font-lock-function-name-face

 subroutine op_concat()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^ font-lock-function-name-face
!                    ^^ f90-ts-font-lock-bracket-face
      str = "abc" // "def" // get_name()
!     ^^^ nil
!         ^ f90-ts-font-lock-operator-face
!           ^^^^^ font-lock-string-face
!                 ^^ f90-ts-font-lock-operator-face
!                    ^^^^^ font-lock-string-face
!                          ^^ f90-ts-font-lock-operator-face
!                             ^^^^^^^^ nil
!                                     ^^ f90-ts-font-lock-bracket-face
 end subroutine op_concat
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^ font-lock-function-name-face

 subroutine op_relation()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^ font-lock-function-name-face
!                      ^^ f90-ts-font-lock-bracket-face
      print *, 6<5, 6>5, 6<=5, 6>=5, 6==5, 6/=5
!     ^^^^^ font-lock-builtin-face
!           ^ nil
!            ^ f90-ts-font-lock-delimiter-face
!              ^ font-lock-number-face
!               ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-delimiter-face
!                   ^ font-lock-number-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^ font-lock-number-face
!                         ^^ f90-ts-font-lock-operator-face
!                           ^ font-lock-number-face
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ font-lock-number-face
!                               ^^ f90-ts-font-lock-operator-face
!                                 ^ font-lock-number-face
!                                  ^ f90-ts-font-lock-delimiter-face
!                                    ^ font-lock-number-face
!                                     ^^ f90-ts-font-lock-operator-face
!                                       ^ font-lock-number-face
!                                        ^ f90-ts-font-lock-delimiter-face
!                                          ^ font-lock-number-face
!                                           ^^ f90-ts-font-lock-operator-face
!                                             ^ font-lock-number-face
      compare1 = 1 .lt. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
      compare2 = 1 .gt. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
      compare3 = 1 .le. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
      compare4 = 1 .ge. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
      compare5 = 1 .eq. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
      compare6 = 1 .ne. 2
!     ^^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                  ^^^^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
 end subroutine op_relation
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^^ font-lock-function-name-face

 subroutine op_unary()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^ font-lock-function-name-face
!                   ^^ f90-ts-font-lock-bracket-face
      i = -j
!     ^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^ f90-ts-font-lock-operator-face
!          ^ nil
      flag = .not. fun(x, y, z)
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^^^^^ f90-ts-font-lock-operator-face
!                  ^^^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^ nil
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
      max_l = .eigenvalmax. mat_A
!     ^^^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^^^^^^^^^^^^^ f90-ts-font-lock-operator-face
!                           ^^^^^ nil
 end subroutine op_unary
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^ font-lock-function-name-face
