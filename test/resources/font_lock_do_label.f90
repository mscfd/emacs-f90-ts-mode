 subroutine once_upon_a_time()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                           ^^ f90-ts-font-lock-bracket-face
      goto 10
!     ^^^^ font-lock-keyword-face
!          ^^ font-lock-constant-face
      if (just_ugly()) then
!     ^^ font-lock-keyword-face
!        ^ f90-ts-font-lock-bracket-face
!         ^^^^^^^^^ nil
!                  ^^^ f90-ts-font-lock-bracket-face
!                      ^^^^ font-lock-keyword-face
           do 100, i = 1,10
!          ^^ font-lock-keyword-face
!             ^^^ font-lock-constant-face
!                ^ f90-ts-font-lock-delimiter-face
!                  ^ nil
!                    ^ f90-ts-font-lock-operator-face
!                      ^ font-lock-number-face
!                       ^ f90-ts-font-lock-delimiter-face
!                        ^^ font-lock-number-face
                do 1000, j = 1,10
!               ^^ font-lock-keyword-face
!                  ^^^^ font-lock-constant-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^ font-lock-number-face
!                             ^ f90-ts-font-lock-delimiter-face
!                              ^^ font-lock-number-face
                     do 1000, k = 1,10
!                    ^^ font-lock-keyword-face
!                       ^^^^ font-lock-constant-face
!                           ^ f90-ts-font-lock-delimiter-face
!                             ^ nil
!                               ^ f90-ts-font-lock-operator-face
!                                 ^ font-lock-number-face
!                                  ^ f90-ts-font-lock-delimiter-face
!                                   ^^ font-lock-number-face
                          do 1000, m = 1,10
!                         ^^ font-lock-keyword-face
!                            ^^^^ font-lock-constant-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^ nil
!                                    ^ f90-ts-font-lock-operator-face
!                                      ^ font-lock-number-face
!                                       ^ f90-ts-font-lock-delimiter-face
!                                        ^^ font-lock-number-face
                               call neither_good_nor_bad()
!                              ^^^^ font-lock-keyword-face
!                                   ^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                                                       ^^ f90-ts-font-lock-bracket-face
 1000           continue  ! comment
!^^^^ font-lock-constant-face
!               ^^^^^^^^ font-lock-keyword-face
!                         ^^^^^^^^^ font-lock-comment-face
 100       continue
!^^^ font-lock-constant-face
!          ^^^^^^^^ font-lock-keyword-face
      end if
!     ^^^ font-lock-keyword-face
!         ^^ font-lock-keyword-face
 10   continue
!^^ font-lock-constant-face
!     ^^^^^^^^ font-lock-keyword-face
 end subroutine once_upon_a_time
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^^^^^^^ font-lock-function-name-face
