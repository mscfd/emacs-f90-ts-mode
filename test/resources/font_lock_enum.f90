 module test_enum
!^^^^^^ font-lock-keyword-face
!       ^^^^^^^^^ font-lock-function-name-face
  ENUM, BIND(C)
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-delimiter-face
!       ^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^ nil
!             ^ f90-ts-font-lock-bracket-face
       ENUMERATOR :: MON = 1
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
!                        ^ f90-ts-font-lock-operator-face
!                          ^ font-lock-number-face
       ! use auto-incremented
!      ^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       ENUMERATOR :: TUE
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
       ENUMERATOR :: WED
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
       ENUMERATOR :: THU
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
       ENUMERATOR :: FRI
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
       ENUMERATOR :: SAT, SUN    ! = 6, 7
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^ nil
!                                ^^^^^^^^ font-lock-comment-face
  END ENUM   ! note: no name for enum statements
! ^^^ font-lock-keyword-face
!     ^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face

  Enumeration Type, Public :: method_e
! ^^^^^^^^^^^ font-lock-keyword-face
!             ^^^^ font-lock-keyword-face
!                 ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^^^ font-lock-type-face
       Enumerator :: BDF1 = 1
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^^ nil
!                         ^ f90-ts-font-lock-operator-face
!                           ^ font-lock-number-face
       Enumerator :: BDF2
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^^ nil
       Enumerator :: RK1  = 10
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
!                         ^ f90-ts-font-lock-operator-face
!                           ^^ font-lock-number-face
       Enumerator :: RK2, RK4, &
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^ f90-ts-font-lock-delimiter-face
!                    ^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ f90-ts-font-lock-delimiter-face
              ESDIRK2
!             ^^^^^^^ nil
  End Enumeration Type method_e
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^ font-lock-keyword-face
!                      ^^^^^^^^ nil

  enumeration type :: boundary_condition
! ^^^^^^^^^^^ font-lock-keyword-face
!             ^^^^ font-lock-keyword-face
!                  ^^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^^^^^^^^^^^^^ font-lock-type-face
       enumerator bc_inflow, bc_outflow, &
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^^^^^^ nil
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^ nil
!                                      ^ f90-ts-font-lock-delimiter-face
!                                        ^ f90-ts-font-lock-delimiter-face
              bc_dirichlet, bc_neumann, bc_robin
!             ^^^^^^^^^^^^ nil
!                         ^ f90-ts-font-lock-delimiter-face
!                           ^^^^^^^^^^ nil
!                                     ^ f90-ts-font-lock-delimiter-face
!                                       ^^^^^^^^ nil

       enumerator bc_heatflux, bc_radiation
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^^^^ nil
       enumerator bc_symmetry = 1000
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-operator-face
!                               ^^^^ font-lock-number-face
  end enumeration type boundary_condition
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^ font-lock-keyword-face
!                      ^^^^^^^^^^^^^^^^^^ nil
 end module test_enum
!^^^ font-lock-keyword-face
!    ^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^ font-lock-function-name-face
