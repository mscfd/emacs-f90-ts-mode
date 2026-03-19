 module sym33
!^^^^^^ font-lock-keyword-face
!       ^^^^^ font-lock-function-name-face

  interface symmul
! ^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
       ! all matrices assumed to be symmetric
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       module procedure symmul_mat33_mat33
!      ^^^^^^ font-lock-keyword-face
!             ^^^^^^^^^ font-lock-keyword-face
!                       ^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
  end interface symmul
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face

  interface operator(.symmul.)
! ^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^ font-lock-keyword-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^^^^^^ nil
!                            ^ f90-ts-font-lock-bracket-face
       module procedure symmul_sym6_sym6
!      ^^^^^^ font-lock-keyword-face
!             ^^^^^^^^^ font-lock-keyword-face
!                       ^^^^^^^^^^^^^^^^ font-lock-function-name-face
  end interface operator(.symmul.)
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^ font-lock-keyword-face
!                       ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^^^ nil
!                                ^ f90-ts-font-lock-bracket-face

  interface assignment(=)
! ^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-keyword-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-operator-face
!                       ^ f90-ts-font-lock-bracket-face
       module procedure assign_sym6_sym6
!      ^^^^^^ font-lock-keyword-face
!             ^^^^^^^^^ font-lock-keyword-face
!                       ^^^^^^^^^^^^^^^^ font-lock-function-name-face
  end interface assignment(=)
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-operator-face
!                           ^ f90-ts-font-lock-bracket-face

  abstract interface
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^^^^^^ font-lock-keyword-face
       subroutine sub_ifc(x)
!      ^^^^^^^^^^ font-lock-keyword-face
!                 ^^^^^^^ font-lock-function-name-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ nil
!                          ^ f90-ts-font-lock-bracket-face
            import some_t
!           ^^^^^^ nil
!                  ^^^^^^ nil
            class(some_t), intent(inout) :: x
!           ^^^^^ font-lock-keyword-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^^^^^^ font-lock-type-face
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^^^^^^^ font-lock-keyword-face
!                                        ^^ f90-ts-font-lock-delimiter-face
!                                           ^ nil
       end subroutine sub_ifc
!      ^^^ font-lock-keyword-face
!          ^^^^^^^^^^ font-lock-keyword-face
!                     ^^^^^^^ font-lock-function-name-face
  end interface
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face

 contains
!^^^^^^^^ font-lock-keyword-face
  ! implementation
! ^^^^^^^^^^^^^^^^ font-lock-comment-face
 end module sym33
!^^^ font-lock-keyword-face
!    ^^^^^^ font-lock-keyword-face
!           ^^^^^ font-lock-function-name-face
