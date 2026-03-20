 function outer(a, b, n) result(T)
!^^^^^^^^ font-lock-keyword-face
!         ^^^^^ font-lock-function-name-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^ nil
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^ nil
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^ font-lock-keyword-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ default
!                                ^ f90-ts-font-lock-bracket-face
      integer, intent(in) :: n
!     ^^^^^^^ font-lock-type-face
!            ^ f90-ts-font-lock-delimiter-face
!              ^^^^^^^^^^ font-lock-keyword-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^ nil
      real, intent(in)    :: a(n), b(n)
!     ^^^^ font-lock-type-face
!         ^ f90-ts-font-lock-delimiter-face
!           ^^^^^^^^^^ font-lock-keyword-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                              ^ nil
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^ nil
!                                     ^ f90-ts-font-lock-bracket-face
      real                :: T(n, n)
!     ^^^^ font-lock-type-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                              ^ nil
!                               ^ f90-ts-font-lock-delimiter-face
!                                 ^ nil
!                                  ^ f90-ts-font-lock-bracket-face
      integer             :: i, j
!     ^^^^^^^ font-lock-type-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^ nil
!                             ^ f90-ts-font-lock-delimiter-face
!                               ^ nil
      prod: do concurrent (i = 1:n,&
!     ^^^^ nil
!         ^ f90-ts-font-lock-delimiter-face
!           ^^ font-lock-keyword-face
!              ^^^^^^^^^^ font-lock-keyword-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ nil
!                            ^ f90-ts-font-lock-operator-face
!                              ^ font-lock-number-face
!                               ^ f90-ts-font-lock-delimiter-face
!                                ^ nil
!                                 ^^ f90-ts-font-lock-delimiter-face
             j = 1:n)
!            ^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-delimiter-face
!                  ^ nil
!                   ^ f90-ts-font-lock-bracket-face
           T(i, j) = a(i) * b(j)
!          ^ nil
!           ^ f90-ts-font-lock-bracket-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^ nil
!                ^ f90-ts-font-lock-bracket-face
!                  ^ f90-ts-font-lock-operator-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                         ^ f90-ts-font-lock-operator-face
!                           ^ nil
!                            ^ f90-ts-font-lock-bracket-face
!                             ^ nil
!                              ^ f90-ts-font-lock-bracket-face
      end do prod
!     ^^^ font-lock-keyword-face
!         ^^ font-lock-keyword-face
!            ^^^^ nil
 end function outer
!^^^ font-lock-keyword-face
!    ^^^^^^^^ font-lock-keyword-face
!             ^^^^^ font-lock-function-name-face



 program int23
!^^^^^^^ font-lock-keyword-face
!        ^^^^^ font-lock-function-name-face
  implicit none(external, type)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^^^ font-lock-keyword-face
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^ font-lock-keyword-face
!                             ^ f90-ts-font-lock-bracket-face
  integer, parameter :: n = 8
! ^^^^^^^ font-lock-type-face
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^ font-lock-keyword-face
!                    ^^ f90-ts-font-lock-delimiter-face
!                       ^ nil
!                         ^ f90-ts-font-lock-operator-face
!                           ^ font-lock-number-face
  real :: x(n), &
! ^^^^ font-lock-type-face
!      ^^ f90-ts-font-lock-delimiter-face
!         ^ nil
!          ^ f90-ts-font-lock-bracket-face
!           ^ nil
!            ^ f90-ts-font-lock-bracket-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^ f90-ts-font-lock-delimiter-face
          y(n), &
!         ^ nil
!          ^ f90-ts-font-lock-bracket-face
!           ^ nil
!            ^ f90-ts-font-lock-bracket-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^ f90-ts-font-lock-delimiter-face
          z(n)
!         ^ nil
!          ^ f90-ts-font-lock-bracket-face
!           ^ nil
!            ^ f90-ts-font-lock-bracket-face
  real :: scale, int2, &
! ^^^^ font-lock-type-face
!      ^^ f90-ts-font-lock-delimiter-face
!         ^^^^^ nil
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^ f90-ts-font-lock-delimiter-face
                 int3
!                ^^^^ nil
  real :: tmp, test
! ^^^^ font-lock-type-face
!      ^^ f90-ts-font-lock-delimiter-face
!         ^^^ nil
!            ^ f90-ts-font-lock-delimiter-face
!              ^^^^ nil
  integer :: i
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^ nil

  scale = 1.0 / real(n)
! ^^^^^ nil
!       ^ f90-ts-font-lock-operator-face
!         ^^^ font-lock-number-face
!             ^ f90-ts-font-lock-operator-face
!               ^^^^ font-lock-builtin-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
  test = -1.0
! ^^^^ nil
!      ^ f90-ts-font-lock-operator-face
!        ^ nil
!         ^^^ font-lock-number-face
  int2 = 0.0
! ^^^^ nil
!      ^ f90-ts-font-lock-operator-face
!        ^^^ font-lock-number-face
  int3 = 0.0
! ^^^^ nil
!      ^ f90-ts-font-lock-operator-face
!        ^^^ font-lock-number-face

  integrate: do, concurrent (i = 1:n) default(none) &
! ^^^^^^^^^ nil
!          ^ f90-ts-font-lock-delimiter-face
!            ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^^^^^^^ font-lock-keyword-face
!                           ^ f90-ts-font-lock-bracket-face
!                            ^ nil
!                              ^ f90-ts-font-lock-operator-face
!                                ^ font-lock-number-face
!                                 ^ f90-ts-font-lock-delimiter-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^^^^ font-lock-keyword-face
!                                            ^ f90-ts-font-lock-bracket-face
!                                             ^^^^ font-lock-keyword-face
!                                                 ^ f90-ts-font-lock-bracket-face
!                                                   ^ f90-ts-font-lock-delimiter-face
         shared(x, y, z, scale) &
!        ^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^ nil
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^ nil
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                               ^ f90-ts-font-lock-delimiter-face
         local(tmp) &
!        ^^^^^ font-lock-keyword-face
!             ^ f90-ts-font-lock-bracket-face
!              ^^^ nil
!                 ^ f90-ts-font-lock-bracket-face
!                   ^ f90-ts-font-lock-delimiter-face
         local_init(test) &
!        ^^^^^^^^^^ font-lock-keyword-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                         ^ f90-ts-font-lock-delimiter-face
         reduce(+: int2, &
!        ^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^ nil
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^ f90-ts-font-lock-delimiter-face
         int3)
!        ^^^^ nil
!            ^ f90-ts-font-lock-bracket-face
       tmp  = (real(i)-0.5) * scale
!      ^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^ f90-ts-font-lock-bracket-face
!              ^^^^ font-lock-builtin-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^ nil
!                    ^ f90-ts-font-lock-bracket-face
!                     ^ f90-ts-font-lock-operator-face
!                      ^^^ font-lock-number-face
!                         ^ f90-ts-font-lock-bracket-face
!                           ^ f90-ts-font-lock-operator-face
!                             ^^^^^ nil
       x(i) = tmp
!      ^ nil
!       ^ f90-ts-font-lock-bracket-face
!        ^ nil
!         ^ f90-ts-font-lock-bracket-face
!           ^ f90-ts-font-lock-operator-face
!             ^^^ nil
       y(i) = tmp**2
!      ^ nil
!       ^ f90-ts-font-lock-bracket-face
!        ^ nil
!         ^ f90-ts-font-lock-bracket-face
!           ^ f90-ts-font-lock-operator-face
!             ^^^ nil
!                ^^ f90-ts-font-lock-operator-face
!                  ^ font-lock-number-face
       z(i) = tmp**3
!      ^ nil
!       ^ f90-ts-font-lock-bracket-face
!        ^ nil
!         ^ f90-ts-font-lock-bracket-face
!           ^ f90-ts-font-lock-operator-face
!             ^^^ nil
!                ^^ f90-ts-font-lock-operator-face
!                  ^ font-lock-number-face
       int2 = int2 + y(i)
!      ^^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ nil
!                       ^ f90-ts-font-lock-bracket-face
       int3 = int3 + z(i)
!      ^^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ nil
!                       ^ f90-ts-font-lock-bracket-face
  end do integrate
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face
!        ^^^^^^^^^ nil
 end program int23
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^ font-lock-function-name-face
