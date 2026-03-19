 subroutine select_case()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^ font-lock-function-name-face
!                      ^^ f90-ts-font-lock-bracket-face
      select_variant: select case (variant)
!     ^^^^^^^^^^^^^^ nil
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^ font-lock-keyword-face
!                            ^^^^ font-lock-keyword-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^^ nil
!                                         ^ f90-ts-font-lock-bracket-face
      case ('version1')
!     ^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^^^^ font-lock-string-face
!                     ^ f90-ts-font-lock-bracket-face
           print *, 'version1'
!          ^^^^^ font-lock-builtin-face
!                ^ nil
!                 ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^ font-lock-string-face
      case ('version2', &
!     ^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^^^^ font-lock-string-face
!                     ^ f90-ts-font-lock-delimiter-face
!                       ^ f90-ts-font-lock-delimiter-face
             'extra')
!            ^^^^^^^ font-lock-string-face
!                   ^ f90-ts-font-lock-bracket-face
           call xyz()
!          ^^^^ font-lock-keyword-face
!               ^^^ font-lock-function-name-face
!                  ^^ f90-ts-font-lock-bracket-face
      case default select_variant
!     ^^^^ font-lock-keyword-face
!          ^^^^^^^ font-lock-keyword-face
!                  ^^^^^^^^^^^^^^ nil
           call uvw()
!          ^^^^ font-lock-keyword-face
!               ^^^ font-lock-function-name-face
!                  ^^ f90-ts-font-lock-bracket-face
      end select select_variant
!     ^^^ font-lock-keyword-face
!         ^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^^^^ nil
 end subroutine select_case
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^^ font-lock-function-name-face


 subroutine select_type()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^ font-lock-function-name-face
!                      ^^ f90-ts-font-lock-bracket-face
      select_arg: select type (arg)
!     ^^^^^^^^^^ nil
!               ^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^ font-lock-keyword-face
!                        ^^^^ font-lock-keyword-face
!                             ^ f90-ts-font-lock-bracket-face
!                              ^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
      type is (integer) select_arg
!     ^^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
!             ^ f90-ts-font-lock-bracket-face
!              ^^^^^^^ font-lock-type-face
!                     ^ f90-ts-font-lock-bracket-face
!                       ^^^^^^^^^^ nil
           arg = 1
!          ^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
      type is (string_t) select_arg
!     ^^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
!             ^ f90-ts-font-lock-bracket-face
!              ^^^^^^^^ font-lock-type-face
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^^^^^ nil
           arg = '1'
!          ^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^^^ font-lock-string-face
      class is (tuple_t) select_arg
!     ^^^^^ font-lock-keyword-face
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^^ font-lock-type-face
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^^^^^ nil
           call arg%set(1, 1, 1)
!          ^^^^ font-lock-keyword-face
!               ^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                   ^^^ font-lock-function-name-face
!                      ^ f90-ts-font-lock-bracket-face
!                       ^ font-lock-number-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^ font-lock-number-face
!                           ^ f90-ts-font-lock-delimiter-face
!                             ^ font-lock-number-face
!                              ^ f90-ts-font-lock-bracket-face
      class default
!     ^^^^^ font-lock-keyword-face
!           ^^^^^^^ font-lock-keyword-face
           error stop 'not supported'
!          ^^^^^ nil
!                ^^^^ nil
!                     ^^^^^^^^^^^^^^^ font-lock-string-face
      end select select_arg
!     ^^^ font-lock-keyword-face
!         ^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^ nil
 end subroutine select_type
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^^ font-lock-function-name-face


 impure real function product_ab(a, b)
!^^^^^^ font-lock-keyword-face
!       ^^^^ font-lock-type-face
!            ^^^^^^^^ font-lock-keyword-face
!                     ^^^^^^^^^^ font-lock-function-name-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ nil
!                                 ^ f90-ts-font-lock-delimiter-face
!                                   ^ nil
!                                    ^ f90-ts-font-lock-bracket-face
      real, dimension(..), intent(in) :: a, b
!     ^^^^ font-lock-type-face
!         ^ f90-ts-font-lock-delimiter-face
!           ^^^^^^^^^^^^^ font-lock-keyword-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^^^^ font-lock-keyword-face
!                                     ^^ f90-ts-font-lock-delimiter-face
!                                        ^ nil
!                                         ^ f90-ts-font-lock-delimiter-face
!                                           ^ nil

      select rank (b)
!     ^^^^^^ font-lock-keyword-face
!            ^^^^ font-lock-keyword-face
!                 ^ f90-ts-font-lock-bracket-face
!                  ^ nil
!                   ^ f90-ts-font-lock-bracket-face
      rank (*)
!     ^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^ nil
!            ^ f90-ts-font-lock-bracket-face
           error stop 'assumed-size input not supported (b)'
!          ^^^^^ nil
!                ^^^^ nil
!                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
      end select
!     ^^^ font-lock-keyword-face
!         ^^^^^^ font-lock-keyword-face

      if (rank(a) < rank(b)) then
!     ^^ font-lock-keyword-face
!        ^ f90-ts-font-lock-bracket-face
!         ^^^^ font-lock-builtin-face
!             ^ f90-ts-font-lock-bracket-face
!              ^ nil
!               ^ f90-ts-font-lock-bracket-face
!                 ^ f90-ts-font-lock-operator-face
!                   ^^^^ font-lock-builtin-face
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ nil
!                         ^^ f90-ts-font-lock-bracket-face
!                            ^^^^ font-lock-keyword-face
           print *, 'rank(a), rank(b) = ', rank(a), rank(b)
!          ^^^^^ font-lock-builtin-face
!                ^ nil
!                 ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                        ^ f90-ts-font-lock-delimiter-face
!                                          ^^^^ font-lock-builtin-face
!                                              ^ f90-ts-font-lock-bracket-face
!                                               ^ nil
!                                                ^ f90-ts-font-lock-bracket-face
!                                                 ^ f90-ts-font-lock-delimiter-face
!                                                   ^^^^ font-lock-builtin-face
!                                                       ^ f90-ts-font-lock-bracket-face
!                                                        ^ nil
!                                                         ^ f90-ts-font-lock-bracket-face
           error stop 'rank size combination of a and b not supported'
!          ^^^^^ nil
!                ^^^^ nil
!                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
      end if
!     ^^^ font-lock-keyword-face
!         ^^ font-lock-keyword-face

      rank_a: select rank (a)
!     ^^^^^^ nil
!           ^ f90-ts-font-lock-delimiter-face
!             ^^^^^^ font-lock-keyword-face
!                    ^^^^ font-lock-keyword-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ nil
!                           ^ f90-ts-font-lock-bracket-face
      rank(0) rank_a
!     ^^^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^ font-lock-number-face
!           ^ f90-ts-font-lock-bracket-face
!             ^^^^^^ nil
           rank_b: select rank (b)
!          ^^^^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^ font-lock-keyword-face
!                         ^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face
           rank (0) rank_b
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ nil
                product_ab = a*b
!               ^^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^ nil
!                             ^ f90-ts-font-lock-operator-face
!                              ^ nil
           rank default rank_b
!          ^^^^ font-lock-keyword-face
!               ^^^^^^^ font-lock-keyword-face
!                       ^^^^^^ nil
                call assert_false()
!               ^^^^ font-lock-keyword-face
!                    ^^^^^^^^^^^^ font-lock-function-name-face
!                                ^^ f90-ts-font-lock-bracket-face
           end select rank_b
!          ^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-keyword-face
!                     ^^^^^^ nil

      rank(1) rank_a
!     ^^^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^ font-lock-number-face
!           ^ f90-ts-font-lock-bracket-face
!             ^^^^^^ nil
           rank_b: select rank (b)
!          ^^^^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^ font-lock-keyword-face
!                         ^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face
           rank (0) rank_b
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ nil
                product_ab = sum(a*b)
!               ^^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^^^ font-lock-builtin-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ nil
!                                 ^ f90-ts-font-lock-operator-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
           rank (1) rank_b
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ nil
                product_ab = dot_product(a, b)
!               ^^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^^^^^^^^^^^ font-lock-builtin-face
!                                       ^ f90-ts-font-lock-bracket-face
!                                        ^ nil
!                                         ^ f90-ts-font-lock-delimiter-face
!                                           ^ nil
!                                            ^ f90-ts-font-lock-bracket-face
           rank default rank_b
!          ^^^^ font-lock-keyword-face
!               ^^^^^^^ font-lock-keyword-face
!                       ^^^^^^ nil
                call assert_false()
!               ^^^^ font-lock-keyword-face
!                    ^^^^^^^^^^^^ font-lock-function-name-face
!                                ^^ f90-ts-font-lock-bracket-face
           end select rank_b
!          ^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-keyword-face
!                     ^^^^^^ nil

      rank(2) rank_a
!     ^^^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^ font-lock-number-face
!           ^ f90-ts-font-lock-bracket-face
!             ^^^^^^ nil
           rank_b: select rank (b)
!          ^^^^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^ font-lock-keyword-face
!                         ^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face
           rank (0) rank_b
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ nil
                product_ab = sum(a*b)
!               ^^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^^^ font-lock-builtin-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ nil
!                                 ^ f90-ts-font-lock-operator-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
           rank (2) rank_b
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ nil
                product_ab = colon_product(a, b)
!               ^^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-operator-face
!                            ^^^^^^^^^^^^^ nil
!                                         ^ f90-ts-font-lock-bracket-face
!                                          ^ nil
!                                           ^ f90-ts-font-lock-delimiter-face
!                                             ^ nil
!                                              ^ f90-ts-font-lock-bracket-face
           rank default rank_b
!          ^^^^ font-lock-keyword-face
!               ^^^^^^^ font-lock-keyword-face
!                       ^^^^^^ nil
                print *, 'rank combination not yet implemented'
!               ^^^^^ font-lock-builtin-face
!                     ^ nil
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
           end select rank_b
!          ^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-keyword-face
!                     ^^^^^^ nil

      rank (*) rank_a
!     ^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^ nil
!            ^ f90-ts-font-lock-bracket-face
!              ^^^^^^ nil
           error stop 'assumed-size input not supported (a)'
!          ^^^^^ nil
!                ^^^^ nil
!                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
      end select rank_a
!     ^^^ font-lock-keyword-face
!         ^^^^^^ font-lock-keyword-face
!                ^^^^^^ nil

 end function product_ab
!^^^ font-lock-keyword-face
!    ^^^^^^^^ font-lock-keyword-face
!             ^^^^^^^^^^ font-lock-function-name-face
