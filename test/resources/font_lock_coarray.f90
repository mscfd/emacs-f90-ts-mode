 program coarray_test
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^^^^ font-lock-function-name-face
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face

  integer :: scalar_co[*]
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^^^^^^^^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ nil
!                       ^ f90-ts-font-lock-bracket-face
  real    :: arr(10)[2, *]
! ^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^^ nil
!               ^ f90-ts-font-lock-bracket-face
!                ^^ font-lock-number-face
!                  ^^ f90-ts-font-lock-bracket-face
!                    ^ font-lock-number-face
!                     ^ f90-ts-font-lock-delimiter-face
!                       ^ nil
!                        ^ f90-ts-font-lock-bracket-face
  real, codimension[2,*], allocatable :: arr2(10)
! ^^^^ font-lock-type-face
!     ^ f90-ts-font-lock-delimiter-face
!       ^^^^^^^^^^^^^^^^ font-lock-keyword-face
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^^ font-lock-keyword-face
!                                     ^^ f90-ts-font-lock-delimiter-face
!                                        ^^^^ nil
!                                            ^ f90-ts-font-lock-bracket-face
!                                             ^^ font-lock-number-face
!                                               ^ f90-ts-font-lock-bracket-face
  real, dimension(1:10), codimension[1:2,*], pointer :: arr3
! ^^^^ font-lock-type-face
!     ^ f90-ts-font-lock-delimiter-face
!       ^^^^^^^^^^^^^^^ font-lock-keyword-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^^^^^^^^^^^^^^ font-lock-keyword-face
!                                          ^ f90-ts-font-lock-delimiter-face
!                                            ^^^^^^^ font-lock-keyword-face
!                                                    ^^ f90-ts-font-lock-delimiter-face
!                                                       ^^^^ nil
  integer, codimension[1:3,1:4,*] :: val
! ^^^^^^^ font-lock-type-face
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^^^^^^^^^^^^^ font-lock-keyword-face
!                                 ^^ f90-ts-font-lock-delimiter-face
!                                    ^^^ nil
  integer :: team_value[*]
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^^^^^^^^^ nil
!                      ^ f90-ts-font-lock-bracket-face
!                       ^ nil
!                        ^ f90-ts-font-lock-bracket-face
  type(lock_type), codimension[*] :: mylock
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^ font-lock-type-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^^^^^^^^^ font-lock-keyword-face
!                                 ^^ f90-ts-font-lock-delimiter-face
!                                    ^^^^^^ nil

  scalar_co[1]       = 42
! ^^^^^^^^^ nil
!          ^ f90-ts-font-lock-bracket-face
!           ^ font-lock-number-face
!            ^ f90-ts-font-lock-bracket-face
!                    ^ f90-ts-font-lock-operator-face
!                      ^^ font-lock-number-face
  arr(5)[1, 2]       = 3.14
! ^^^ nil
!    ^ f90-ts-font-lock-bracket-face
!     ^ font-lock-number-face
!      ^^ f90-ts-font-lock-bracket-face
!        ^ font-lock-number-face
!         ^ f90-ts-font-lock-delimiter-face
!           ^ font-lock-number-face
!            ^ f90-ts-font-lock-bracket-face
!                    ^ f90-ts-font-lock-operator-face
!                      ^^^^ font-lock-number-face
  val[this_image()]  = this_image()
! ^^^ nil
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^^^^^^^ font-lock-builtin-face
!               ^^^ f90-ts-font-lock-bracket-face
!                    ^ f90-ts-font-lock-operator-face
!                      ^^^^^^^^^^ font-lock-builtin-face
!                                ^^ f90-ts-font-lock-bracket-face

  ! intrinsics
! ^^^^^^^^^^^^ font-lock-comment-face
  print *, num_images()
! ^^^^^ font-lock-builtin-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^ font-lock-builtin-face
!                    ^^ f90-ts-font-lock-bracket-face
  print *, this_image()
! ^^^^^ font-lock-builtin-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^ font-lock-builtin-face
!                    ^^ f90-ts-font-lock-bracket-face
  print *, image_index(arr, [1, 2])
! ^^^^^ font-lock-builtin-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-delimiter-face
!                           ^ f90-ts-font-lock-bracket-face
!                            ^ font-lock-number-face
!                             ^ f90-ts-font-lock-delimiter-face
!                               ^ font-lock-number-face
!                                ^^ f90-ts-font-lock-bracket-face

  ! synchronization
! ^^^^^^^^^^^^^^^^^ font-lock-comment-face
  sync all
! ^^^^ font-lock-keyword-face
!      ^^^ font-lock-keyword-face
  sync images(*)
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-keyword-face
!            ^ f90-ts-font-lock-bracket-face
!             ^ nil
!              ^ f90-ts-font-lock-bracket-face
  sync images([1, 2, 3])
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-keyword-face
!            ^^ f90-ts-font-lock-bracket-face
!              ^ font-lock-number-face
!               ^ f90-ts-font-lock-delimiter-face
!                 ^ font-lock-number-face
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^ font-lock-number-face
!                     ^^ f90-ts-font-lock-bracket-face
  sync memory
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-keyword-face

  ! critical section
! ^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  name_crit: critical
! ^^^^^^^^^ nil
!          ^ f90-ts-font-lock-delimiter-face
!            ^^^^^^^^ font-lock-keyword-face
       scalar_co[1] = scalar_co[1] + 1
!      ^^^^^^^^^ nil
!               ^ f90-ts-font-lock-bracket-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^ f90-ts-font-lock-operator-face
!                     ^^^^^^^^^ nil
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ font-lock-number-face
!                                ^ f90-ts-font-lock-bracket-face
!                                  ^ f90-ts-font-lock-operator-face
!                                    ^ font-lock-number-face
  end critical name_crit
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^^^^ nil

  ! team
! ^^^^^^ font-lock-comment-face
  form team (mod(this_image(), 2) + 1, team_value)
! ^^^^ font-lock-keyword-face
!      ^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^ font-lock-builtin-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^^^^^^ font-lock-builtin-face
!                          ^^ f90-ts-font-lock-bracket-face
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ font-lock-number-face
!                               ^ f90-ts-font-lock-bracket-face
!                                 ^ f90-ts-font-lock-operator-face
!                                   ^ font-lock-number-face
!                                    ^ f90-ts-font-lock-delimiter-face
!                                      ^^^^^^^^^^ nil
!                                                ^ f90-ts-font-lock-bracket-face
  name_ch: change team (team_value)
! ^^^^^^^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^ font-lock-keyword-face
!                 ^^^^ font-lock-keyword-face
!                      ^ f90-ts-font-lock-bracket-face
!                       ^^^^^^^^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
       print *, "in team, image:", this_image()
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^ font-lock-builtin-face
!                                            ^^ f90-ts-font-lock-bracket-face
  end change team name_ch
! ^^^ font-lock-keyword-face
!     ^^^^^^ font-lock-keyword-face
!            ^^^^ font-lock-keyword-face
!                 ^^^^^^^ nil

 end program coarray_test
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^ font-lock-function-name-face


 program sum_square
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^^ font-lock-function-name-face
  use, intrinsic :: iso_fortran_env, only: lock_type
! ^^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-delimiter-face
!      ^^^^^^^^^ nil
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^ nil
!                                  ^ f90-ts-font-lock-delimiter-face
!                                    ^^^^ font-lock-keyword-face
!                                        ^ f90-ts-font-lock-delimiter-face
!                                          ^^^^^^^^^ nil
  use iso_fortran_env, only : STAT_FAILED_IMAGE,  STAT_STOPPED_IMAGE
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^ font-lock-keyword-face
!                           ^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^^^^^^^^^^^^ nil
!                                              ^ f90-ts-font-lock-delimiter-face
!                                                 ^^^^^^^^^^^^^^^^^^ nil

  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face
  type(lock_type), codimension[*] :: slock
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^ font-lock-type-face
!               ^ f90-ts-font-lock-bracket-face
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^^^^^^^^^ font-lock-keyword-face
!                                 ^^ f90-ts-font-lock-delimiter-face
!                                    ^^^^^ nil
  integer, codimension[*] :: ssq
! ^^^^^^^ font-lock-type-face
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^^^^^ font-lock-keyword-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^^^ nil
  integer :: sync_stat, alloc_stat
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^^^^^^^^ nil
!                     ^ f90-ts-font-lock-delimiter-face
!                       ^^^^^^^^^^ nil

  if (this_image() == 1) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^^^^^^^ font-lock-builtin-face
!               ^^ f90-ts-font-lock-bracket-face
!                  ^^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^ font-lock-keyword-face
       i = 1
!      ^ nil
!        ^ f90-ts-font-lock-operator-face
!          ^ font-lock-number-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face
  sync all (stat=sync_stat)
! ^^^^ font-lock-keyword-face
!      ^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^^^^ nil
!                         ^ f90-ts-font-lock-bracket-face
  if (stat /= 0) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^ nil
!          ^^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!              ^ f90-ts-font-lock-bracket-face
!                ^^^^ font-lock-keyword-face
       if (stat == STAT_FAILED_IMAGE) then
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^^^ nil
!               ^^ f90-ts-font-lock-operator-face
!                  ^^^^^^^^^^^^^^^^^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^ font-lock-keyword-face
            print *,"Failed images: ", failed_images()
!           ^^^^^ font-lock-builtin-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                    ^ f90-ts-font-lock-delimiter-face
!                                      ^^^^^^^^^^^^^ font-lock-builtin-face
!                                                   ^^ f90-ts-font-lock-bracket-face
       else if (stat == STAT_STOPPED_IMAGE) then
!      ^^^^ font-lock-keyword-face
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ nil
!                    ^^ f90-ts-font-lock-operator-face
!                       ^^^^^^^^^^^^^^^^^^ nil
!                                         ^ f90-ts-font-lock-bracket-face
!                                           ^^^^ font-lock-keyword-face
            print *,"Stopped images: ", stopped_images()
!           ^^^^^ font-lock-builtin-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                     ^ f90-ts-font-lock-delimiter-face
!                                       ^^^^^^^^^^^^^^ font-lock-builtin-face
!                                                     ^^ f90-ts-font-lock-bracket-face
       else
!      ^^^^ font-lock-keyword-face
            print *,"Unforseen error, aborting"
!           ^^^^^ font-lock-builtin-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
            error stop
!           ^^^^^ font-lock-keyword-face
!                 ^^^^ font-lock-keyword-face
       end if
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face

  block
! ^^^^^ font-lock-keyword-face
       logical :: gotit
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^ nil
       do
!      ^^ font-lock-keyword-face
            lock(slock[1], acquired_lock=gotit)
!           ^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                      ^ font-lock-number-face
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^^^^^^^ nil
!                                       ^ f90-ts-font-lock-operator-face
!                                        ^^^^^ nil
!                                             ^ f90-ts-font-lock-bracket-face
            if (gotit) exit
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^ nil
!                    ^ f90-ts-font-lock-bracket-face
!                      ^^^^ font-lock-keyword-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end block
! ^^^ font-lock-keyword-face
!     ^^^^^ font-lock-keyword-face
  ssq[1] = this_image()**2 + ssq[1]
! ^^^ nil
!    ^ f90-ts-font-lock-bracket-face
!     ^ font-lock-number-face
!      ^ f90-ts-font-lock-bracket-face
!        ^ f90-ts-font-lock-operator-face
!          ^^^^^^^^^^ font-lock-builtin-face
!                    ^^ f90-ts-font-lock-bracket-face
!                      ^^ f90-ts-font-lock-operator-face
!                        ^ font-lock-number-face
!                          ^ f90-ts-font-lock-operator-face
!                            ^^^ nil
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ font-lock-number-face
!                                 ^ f90-ts-font-lock-bracket-face
  unlock (slock[1])
! ^^^^^^ font-lock-keyword-face
!        ^ f90-ts-font-lock-bracket-face
!         ^^^^^ nil
!              ^ f90-ts-font-lock-bracket-face
!               ^ font-lock-number-face
!                ^^ f90-ts-font-lock-bracket-face
  call co_broadcast(ssq, 1)
! ^^^^ font-lock-keyword-face
!      ^^^^^^^^^^^^ font-lock-builtin-face
!                  ^ f90-ts-font-lock-bracket-face
!                   ^^^ nil
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^ font-lock-number-face
!                         ^ f90-ts-font-lock-bracket-face
  print *, this_image(), ssq
! ^^^^^ font-lock-builtin-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^ font-lock-builtin-face
!                    ^^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^ nil

  ssq = this_image()**2
! ^^^ nil
!     ^ f90-ts-font-lock-operator-face
!       ^^^^^^^^^^ font-lock-builtin-face
!                 ^^ f90-ts-font-lock-bracket-face
!                   ^^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
  call co_sum(ssq, result_image=1)
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-builtin-face
!            ^ f90-ts-font-lock-bracket-face
!             ^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^^^^^^^ nil
!                              ^ f90-ts-font-lock-operator-face
!                               ^ font-lock-number-face
!                                ^ f90-ts-font-lock-bracket-face
  call co_min(ssq)
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-builtin-face
!            ^ f90-ts-font-lock-bracket-face
!             ^^^ nil
!                ^ f90-ts-font-lock-bracket-face
  call co_max(ssq)
! ^^^^ font-lock-keyword-face
!      ^^^^^^ font-lock-builtin-face
!            ^ f90-ts-font-lock-bracket-face
!             ^^^ nil
!                ^ f90-ts-font-lock-bracket-face
  call co_reduce(ssq, redfun)
! ^^^^ font-lock-keyword-face
!      ^^^^^^^^^ font-lock-builtin-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^ nil
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^^^ nil
!                           ^ f90-ts-font-lock-bracket-face

  print *, this_image(), co_min(ssq), co_max(ssq)
! ^^^^^ font-lock-builtin-face
!       ^ nil
!        ^ f90-ts-font-lock-delimiter-face
!          ^^^^^^^^^^ font-lock-builtin-face
!                    ^^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-delimiter-face
!                        ^^^^^^ font-lock-builtin-face
!                              ^ f90-ts-font-lock-bracket-face
!                               ^^^ nil
!                                  ^ f90-ts-font-lock-bracket-face
!                                   ^ f90-ts-font-lock-delimiter-face
!                                     ^^^^^^ font-lock-builtin-face
!                                           ^ f90-ts-font-lock-bracket-face
!                                            ^^^ nil
!                                               ^ f90-ts-font-lock-bracket-face
  if (this_image() == 1) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^^^^^^^ font-lock-builtin-face
!               ^^ f90-ts-font-lock-bracket-face
!                  ^^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
!                      ^ f90-ts-font-lock-bracket-face
!                        ^^^^ font-lock-keyword-face
       print *, this_image(), co_sum(ssq, result_image=1)
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-builtin-face
!                         ^^ f90-ts-font-lock-bracket-face
!                           ^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^ font-lock-builtin-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^^^ nil
!                                       ^ f90-ts-font-lock-delimiter-face
!                                         ^^^^^^^^^^^^ nil
!                                                     ^ f90-ts-font-lock-operator-face
!                                                      ^ font-lock-number-face
!                                                       ^ f90-ts-font-lock-bracket-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face
 end program sum_square
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^ font-lock-function-name-face


 program event_with_wait
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^^^^^^^ font-lock-function-name-face
  use iso_fortran_env, only: event_type
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^ nil
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face

  type(event_type) :: evt[*]
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^^ font-lock-type-face
!                ^ f90-ts-font-lock-bracket-face
!                  ^^ f90-ts-font-lock-delimiter-face
!                     ^^^ nil
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ nil
!                          ^ f90-ts-font-lock-bracket-face
  integer :: me
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^ nil

  me = this_image()
! ^^ nil
!    ^ f90-ts-font-lock-operator-face
!      ^^^^^^^^^^ font-lock-builtin-face
!                ^^ f90-ts-font-lock-bracket-face

  if (num_images() < 2) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^^^^^^^ font-lock-builtin-face
!               ^^ f90-ts-font-lock-bracket-face
!                  ^ f90-ts-font-lock-operator-face
!                    ^ font-lock-number-face
!                     ^ f90-ts-font-lock-bracket-face
!                       ^^^^ font-lock-keyword-face
       if (me == 1) print *, "Run with at least 2 images"
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^ nil
!             ^^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^ font-lock-builtin-face
!                         ^ nil
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
       stop
!      ^^^^ font-lock-keyword-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face

  if (me == 1) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^ nil
!        ^^ f90-ts-font-lock-operator-face
!           ^ font-lock-number-face
!            ^ f90-ts-font-lock-bracket-face
!              ^^^^ font-lock-keyword-face
       print *, "image 1 waiting for TWO events..."
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       ! Wait until at least 2 events have been posted
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       call event_wait(evt, until_count=2)
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-delimiter-face
!                           ^^^^^^^^^^^ nil
!                                      ^ f90-ts-font-lock-operator-face
!                                       ^ font-lock-number-face
!                                        ^ f90-ts-font-lock-bracket-face

       print *, "image 1 received 2 events!"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

  else if (me == 2) then
! ^^^^ font-lock-keyword-face
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^ nil
!             ^^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^ font-lock-keyword-face
       print *, "image 2 posting events..."
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       call event_post(evt[1])
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ font-lock-number-face
!                           ^^ f90-ts-font-lock-bracket-face
       print *, "Posted first event"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       call event_post(evt[1])
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ font-lock-number-face
!                           ^^ f90-ts-font-lock-bracket-face
       print *, "Posted second event"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face

 end program event_with_wait
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^^^ font-lock-function-name-face


 program fail_atom_event_example
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
  use iso_fortran_env, only: event_type, atomic_int_kind
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^ nil
!                                      ^ f90-ts-font-lock-delimiter-face
!                                        ^^^^^^^^^^^^^^^ nil
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ font-lock-keyword-face

  type(event_type) :: evt[*]
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^^ font-lock-type-face
!                ^ f90-ts-font-lock-bracket-face
!                  ^^ f90-ts-font-lock-delimiter-face
!                     ^^^ nil
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ nil
!                          ^ f90-ts-font-lock-bracket-face
  integer(atomic_int_kind) :: flag[*]   ! shared atomic flag
! ^^^^^^^ font-lock-type-face
!        ^ f90-ts-font-lock-bracket-face
!         ^^^^^^^^^^^^^^^ nil
!                        ^ f90-ts-font-lock-bracket-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                       ^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  integer :: me, val
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^ nil
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^ nil
  logical :: done
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^^^^ nil

  me = this_image()
! ^^ nil
!    ^ f90-ts-font-lock-operator-face
!      ^^^^^^^^^^ font-lock-builtin-face
!                ^^ f90-ts-font-lock-bracket-face

  if (num_images() < 2) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^^^^^^^^^ font-lock-builtin-face
!               ^^ f90-ts-font-lock-bracket-face
!                  ^ f90-ts-font-lock-operator-face
!                    ^ font-lock-number-face
!                     ^ f90-ts-font-lock-bracket-face
!                       ^^^^ font-lock-keyword-face
       if (me == 1) print *, "Run with at least 2 images"
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^ nil
!             ^^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^^ font-lock-builtin-face
!                         ^ nil
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
       stop
!      ^^^^ font-lock-keyword-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face

  if (me == 2) then
! ^^ font-lock-keyword-face
!    ^ f90-ts-font-lock-bracket-face
!     ^^ nil
!        ^^ f90-ts-font-lock-operator-face
!           ^ font-lock-number-face
!            ^ f90-ts-font-lock-bracket-face
!              ^^^^ font-lock-keyword-face
       ! producer (image 2)
!      ^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       print *, "image 2: starting work"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       call atomic_define(flag[1], 42)
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^^^ font-lock-builtin-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^^^^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                              ^ font-lock-number-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^ font-lock-number-face
!                                    ^ f90-ts-font-lock-bracket-face
       print *, "image 2: wrote value"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       call event_post(evt[1])
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ font-lock-number-face
!                           ^^ f90-ts-font-lock-bracket-face
       print *, "image 2: notified image 1"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       ! simulate failure after notifying
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       print *, "image 2: failing now"
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
       fail image
!      ^^^^ font-lock-keyword-face
!           ^^^^^ font-lock-keyword-face

  else if (me == 1) then
! ^^^^ font-lock-keyword-face
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^ nil
!             ^^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
!                 ^ f90-ts-font-lock-bracket-face
!                   ^^^^ font-lock-keyword-face
       ! consumer (image 1)
!      ^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       print *, "image 1: waiting for notification..."
!      ^^^^^ font-lock-builtin-face
!            ^ nil
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face

       done = .false.
!      ^^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^^^^^^^ font-lock-constant-face

       do while (.not. done)
!      ^^ font-lock-keyword-face
!         ^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^ f90-ts-font-lock-operator-face
!                      ^^^^ nil
!                          ^ f90-ts-font-lock-bracket-face
            ! wait for event (may return even if image 2 later fails)
!           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            call event_wait(evt)
!           ^^^^ font-lock-keyword-face
!                ^^^^^^^^^^ font-lock-builtin-face
!                          ^ f90-ts-font-lock-bracket-face
!                           ^^^ nil
!                              ^ f90-ts-font-lock-bracket-face

            ! check if image 2 is still alive
!           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            if (this_image() /= 2 .and. failed_images() /= 0) then
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^^^^^ font-lock-builtin-face
!                         ^^ f90-ts-font-lock-bracket-face
!                            ^^ f90-ts-font-lock-operator-face
!                               ^ font-lock-number-face
!                                 ^^^^^ f90-ts-font-lock-operator-face
!                                       ^^^^^^^^^^^^^ font-lock-builtin-face
!                                                    ^^ f90-ts-font-lock-bracket-face
!                                                       ^^ f90-ts-font-lock-operator-face
!                                                          ^ font-lock-number-face
!                                                           ^ f90-ts-font-lock-bracket-face
!                                                             ^^^^ font-lock-keyword-face
                 print *, "image 1: detected a failed image"
!                ^^^^^ font-lock-builtin-face
!                      ^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
            end if
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face

            ! safely read atomic value
!           ^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            call atomic_ref(val, flag)
!           ^^^^ font-lock-keyword-face
!                ^^^^^^^^^^ font-lock-builtin-face
!                          ^ f90-ts-font-lock-bracket-face
!                           ^^^ nil
!                              ^ f90-ts-font-lock-delimiter-face
!                                ^^^^ nil
!                                    ^ f90-ts-font-lock-bracket-face

            print *, "image 1: received value =", val
!           ^^^^^ font-lock-builtin-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                               ^ f90-ts-font-lock-delimiter-face
!                                                 ^^^ nil
            done = .true.
!           ^^^^ nil
!                ^ f90-ts-font-lock-operator-face
!                  ^^^^^^ font-lock-constant-face

       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end if
! ^^^ font-lock-keyword-face
!     ^^ font-lock-keyword-face

 end program fail_atom_event_example
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
