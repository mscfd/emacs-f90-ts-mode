 program program1
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^ font-lock-function-name-face
  write(*,'(a)') 'hello'
! ^^^^^ font-lock-builtin-face
!      ^ f90-ts-font-lock-bracket-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!         ^^^^^ font-lock-string-face
!              ^ f90-ts-font-lock-bracket-face
!                ^^^^^^^ font-lock-string-face
 end program program1
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^ font-lock-function-name-face

 program program2
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^ font-lock-function-name-face
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
  integer :: i
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^ nil
  do i = 1,10
! ^^ font-lock-keyword-face
!    ^ nil
!      ^ f90-ts-font-lock-operator-face
!        ^ font-lock-number-face
!         ^ f90-ts-font-lock-delimiter-face
!          ^^ font-lock-number-face
       print *,'i = ', i
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!              ^^^^^^ font-lock-string-face
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^ nil
  end do
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face
 end program program2
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^ font-lock-function-name-face

 program program3
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^ font-lock-function-name-face
  implicit none (type, external)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^ font-lock-keyword-face
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
  integer :: i
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^ nil
  do i = 1,10
! ^^ font-lock-keyword-face
!    ^ nil
!      ^ f90-ts-font-lock-operator-face
!        ^ font-lock-number-face
!         ^ f90-ts-font-lock-delimiter-face
!          ^^ font-lock-number-face
       call sub(i)
!      ^^^^ font-lock-keyword-face
!           ^^^ font-lock-function-name-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                ^ f90-ts-font-lock-bracket-face
  end do
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face
 contains
!^^^^^^^^ font-lock-keyword-face
  subroutine sub(k)
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^ font-lock-function-name-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ nil
!                 ^ f90-ts-font-lock-bracket-face
       integer, intent(in) :: k
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^ nil
       print *,k
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!              ^ nil
  end subroutine sub
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^ font-lock-function-name-face
 end program program3
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^ font-lock-function-name-face

 module square
!^^^^^^ font-lock-keyword-face
!       ^^^^^^ font-lock-function-name-face
  implicit none (type, &
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^ font-lock-keyword-face
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^ f90-ts-font-lock-delimiter-face
         external)
!        ^^^^^^^^ font-lock-keyword-face
!                ^ f90-ts-font-lock-bracket-face
  private
! ^^^^^^^ font-lock-keyword-face
  public :: get_square
! ^^^^^^ font-lock-keyword-face
!        ^^ f90-ts-font-lock-delimiter-face
!           ^^^^^^^^^^ nil

  interface get_square
! ^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-function-name-face
       module function get_square_impl(x)
!      ^^^^^^ font-lock-keyword-face
!             ^^^^^^^^ font-lock-keyword-face
!                      ^^^^^^^^^^^^^^^ font-lock-function-name-face
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^ nil
!                                       ^ f90-ts-font-lock-bracket-face
            real, intent(in) :: x
!           ^^^^ font-lock-type-face
!               ^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^^ font-lock-keyword-face
!                            ^^ f90-ts-font-lock-delimiter-face
!                               ^ nil
            real :: get_square_impl
!           ^^^^ font-lock-type-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^ nil
       end function get_square_impl
!      ^^^ font-lock-keyword-face
!          ^^^^^^^^ font-lock-keyword-face
!                   ^^^^^^^^^^^^^^^ font-lock-function-name-face
  end interface get_square
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^ font-lock-function-name-face

 contains
!^^^^^^^^ font-lock-keyword-face

  module procedure get_square_impl
! ^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^ font-lock-keyword-face
!                  ^^^^^^^^^^^^^^^ font-lock-function-name-face
       get_square_impl = x**2
!      ^^^^^^^^^^^^^^^ nil
!                      ^ f90-ts-font-lock-operator-face
!                        ^ nil
!                         ^^ f90-ts-font-lock-operator-face
!                           ^ font-lock-number-face
  end procedure get_square_impl
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^^^^^^^^^ font-lock-function-name-face

 end module square
!^^^ font-lock-keyword-face
!    ^^^^^^ font-lock-keyword-face
!           ^^^^^^ font-lock-function-name-face

 submodule (do_mod) do_sub
!^^^^^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^ font-lock-function-name-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ font-lock-function-name-face
 contains
!^^^^^^^^ font-lock-keyword-face
  module Procedure do_fun
! ^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^ font-lock-keyword-face
!                  ^^^^^^ font-lock-function-name-face
       do_fun = 0
!      ^^^^^^ nil
!             ^ f90-ts-font-lock-operator-face
!               ^ font-lock-number-face
       sum_do_: do
!      ^^^^^^^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^ font-lock-keyword-face
            if (do_fun > 1000) exit
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^ nil
!                      ^ f90-ts-font-lock-operator-face
!                        ^^^^ font-lock-number-face
!                            ^ f90-ts-font-lock-bracket-face
!                              ^^^^ font-lock-keyword-face
            i = i + 1
!           ^ nil
!             ^ f90-ts-font-lock-operator-face
!               ^ nil
!                 ^ f90-ts-font-lock-operator-face
!                   ^ font-lock-number-face
            do_fun = do_fun + i**2
!           ^^^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^^^^^^ nil
!                           ^ f90-ts-font-lock-operator-face
!                             ^ nil
!                              ^^ f90-ts-font-lock-operator-face
!                                ^ font-lock-number-face
       end do sum_do_
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
!             ^^^^^^^ nil
  End Procedure do_fun
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^ font-lock-function-name-face
 end submodule do_sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-function-name-face

 submodule (do_mod) do_sub
!^^^^^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^ font-lock-function-name-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ font-lock-function-name-face
 contains
!^^^^^^^^ font-lock-keyword-face
  module recursive subroutine rec(n)
! ^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^ font-lock-keyword-face
!                  ^^^^^^^^^^ font-lock-keyword-face
!                             ^^^ font-lock-function-name-face
!                                ^ f90-ts-font-lock-bracket-face
!                                 ^ nil
!                                  ^ f90-ts-font-lock-bracket-face
       integer, intent(in) :: n
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^ nil
       verify: if (n > 0) then
!      ^^^^^^ nil
!            ^ f90-ts-font-lock-delimiter-face
!              ^^ font-lock-keyword-face
!                 ^ f90-ts-font-lock-bracket-face
!                  ^ nil
!                    ^ f90-ts-font-lock-operator-face
!                      ^ font-lock-number-face
!                       ^ f90-ts-font-lock-bracket-face
!                         ^^^^ font-lock-keyword-face
            call rec(n-1)
!           ^^^^ font-lock-keyword-face
!                ^^^ font-lock-function-name-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^ nil
!                     ^ f90-ts-font-lock-operator-face
!                      ^ font-lock-number-face
!                       ^ f90-ts-font-lock-bracket-face
            print '(10a,i4)', 'done ', n
!           ^^^^^ font-lock-builtin-face
!                 ^^^^^^^^^^ font-lock-string-face
!                           ^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^^ font-lock-string-face
!                                    ^ f90-ts-font-lock-delimiter-face
!                                      ^ nil
       end if verify
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
!             ^^^^^^ nil
  end subroutine rec
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^ font-lock-function-name-face
 end submodule do_sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-function-name-face

 submodule (do_mod) do_sub
!^^^^^^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^ font-lock-function-name-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^ font-lock-function-name-face
 contains
!^^^^^^^^ font-lock-keyword-face
  module impure function do_fun(N)
! ^^^^^^ font-lock-keyword-face
!        ^^^^^^ font-lock-keyword-face
!               ^^^^^^^^ font-lock-keyword-face
!                        ^^^^^^ font-lock-function-name-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face
       integer, intent(in) :: N
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^ nil
       integer :: i
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
       i = 0
!      ^ nil
!        ^ f90-ts-font-lock-operator-face
!          ^ font-lock-number-face
       while_loop:do while (i < n)
!      ^^^^^^^^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                 ^^ font-lock-keyword-face
!                    ^^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-bracket-face
!                           ^ nil
!                             ^ f90-ts-font-lock-operator-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face
            print *, i
!           ^^^^^ font-lock-builtin-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^ nil
       end do while_loop
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
!             ^^^^^^^^^^ nil
  end function do_fun
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-function-name-face
 end submodule do_sub
!^^^ font-lock-keyword-face
!    ^^^^^^^^^ font-lock-keyword-face
!              ^^^^^^ font-lock-function-name-face
