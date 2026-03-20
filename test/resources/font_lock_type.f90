 module mytype
!^^^^^^ font-lock-keyword-face
!       ^^^^^^ font-lock-function-name-face
  use mod, only: sub_ifc, &
! ^^^ font-lock-keyword-face
!     ^^^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^ f90-ts-font-lock-delimiter-face
         other
!        ^^^^^ nil
  implicit none (type, external)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^ font-lock-keyword-face
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
  private
! ^^^^^^^ font-lock-keyword-face

  type, abstract, public :: base_t
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-delimiter-face
!       ^^^^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^ font-lock-keyword-face
!                        ^^ f90-ts-font-lock-delimiter-face
!                           ^^^^^^ font-lock-type-face
       integer, dimension(1:10) :: values
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^ font-lock-keyword-face
!                               ^^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^ nil
       procedure(sub_ifc), nopass, pointer :: sub => null()
!      ^^^^^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^ font-lock-keyword-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^ font-lock-keyword-face
!                                          ^^ f90-ts-font-lock-delimiter-face
!                                             ^^^ nil
!                                                 ^^ f90-ts-font-lock-delimiter-face
!                                                    ^^^^^^ font-lock-constant-face
       class(base_t), pointer :: next => null()
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^ font-lock-type-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^^ font-lock-keyword-face
!                             ^^ f90-ts-font-lock-delimiter-face
!                                ^^^^ nil
!                                     ^^ f90-ts-font-lock-delimiter-face
!                                        ^^^^^^ font-lock-constant-face
  contains
! ^^^^^^^^ font-lock-keyword-face
       procedure(fun_ifc), deferred :: fun
!      ^^^^^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^^ font-lock-keyword-face
!                                   ^^ f90-ts-font-lock-delimiter-face
!                                      ^^^ font-lock-function-name-face
  end type base_t
! ^^^ font-lock-keyword-face
!     ^^^^ font-lock-keyword-face
!          ^^^^^^ font-lock-type-face

  type, extends(base_t), public :: ext_t
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-delimiter-face
!       ^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^ font-lock-type-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^^ font-lock-keyword-face
!                               ^^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^ font-lock-type-face
  contains
! ^^^^^^^^ font-lock-keyword-face
       procedure :: fun => fun_ext
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^ font-lock-function-name-face
!                       ^^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^ font-lock-function-name-face
  end type ext_t
! ^^^ font-lock-keyword-face
!     ^^^^ font-lock-keyword-face
!          ^^^^^ font-lock-type-face

 contains
!^^^^^^^^ font-lock-keyword-face

  function fun_ext(self) result(s)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^^^^ font-lock-function-name-face
!                 ^ f90-ts-font-lock-bracket-face
!                  ^^^^ f90-ts-font-lock-special-var-face
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ default
!                                ^ f90-ts-font-lock-bracket-face
       integer :: s
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
       class(ext_t), intent(in) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^ font-lock-type-face
!                 ^ f90-ts-font-lock-bracket-face
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^^^^^^^^^^ font-lock-keyword-face
!                               ^^ f90-ts-font-lock-delimiter-face
!                                  ^^^^ f90-ts-font-lock-special-var-face
       integer, dimension(1:ubound(self%values)) :: v
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^ font-lock-keyword-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ font-lock-number-face
!                          ^ f90-ts-font-lock-delimiter-face
!                           ^^^^^^ font-lock-builtin-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^^^^ f90-ts-font-lock-special-var-face
!                                      ^ f90-ts-font-lock-operator-face
!                                       ^^^^^^ nil
!                                             ^^ f90-ts-font-lock-bracket-face
!                                                ^^ f90-ts-font-lock-delimiter-face
!                                                   ^ nil
       v = self%values
!      ^ nil
!        ^ f90-ts-font-lock-operator-face
!          ^^^^ f90-ts-font-lock-special-var-face
!              ^ f90-ts-font-lock-operator-face
!               ^^^^^^ nil
       call other(v)
!      ^^^^ font-lock-keyword-face
!           ^^^^^ font-lock-function-name-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^ nil
!                  ^ f90-ts-font-lock-bracket-face
       call self%sub(v)
!      ^^^^ font-lock-keyword-face
!           ^^^^ f90-ts-font-lock-special-var-face
!               ^ f90-ts-font-lock-operator-face
!                ^^^ font-lock-function-name-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
       call self%next%sub(v)
!      ^^^^ font-lock-keyword-face
!           ^^^^ f90-ts-font-lock-special-var-face
!               ^ f90-ts-font-lock-operator-face
!                ^^^^ nil
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^ font-lock-function-name-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ nil
!                          ^ f90-ts-font-lock-bracket-face
       s = sum(v)
!      ^ nil
!        ^ f90-ts-font-lock-operator-face
!          ^^^ font-lock-builtin-face
!             ^ f90-ts-font-lock-bracket-face
!              ^ nil
!               ^ f90-ts-font-lock-bracket-face
  end function fun_ext
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^^ font-lock-function-name-face

 end module mytype
!^^^ font-lock-keyword-face
!    ^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face
