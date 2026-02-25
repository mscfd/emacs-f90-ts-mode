 module collatz_mod
!^^^^^^ font-lock-keyword-face
!       ^^^^^^^^^^^ font-lock-function-name-face
  use iso_fortran_env, only: output_unit
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^ nil
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ nil
  private
! ^^^^^^^ font-lock-keyword-face

  ! Public interfaces
! ^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  public :: collatz_processor, &
! ^^^^^^ font-lock-keyword-face
!        ^^ f90-ts-font-lock-delimiter-face
!           ^^^^^^^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ f90-ts-font-lock-delimiter-face
         collatz_processor_t
!        ^^^^^^^^^^^^^^^^^^^ nil

  ! Type for storing results for a single number
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  type :: collatz_seq_t
! ^^^^ font-lock-keyword-face
!      ^^ f90-ts-font-lock-delimiter-face
!         ^^^^^^^^^^^^^ font-lock-type-face
       integer :: start_number = 0
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^^^^ nil
!                              ^ f90-ts-font-lock-operator-face
!                                ^ font-lock-number-face
       integer :: sequence_length = 0
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^^^^^^^ nil
!                                 ^ f90-ts-font-lock-operator-face
!                                   ^ font-lock-number-face

       !> number of 3n+1 applications
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       integer :: n_odd_steps = 0
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-operator-face
!                               ^ font-lock-number-face
       integer :: max_value = 0
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^ nil
!                           ^ f90-ts-font-lock-operator-face
!                             ^ font-lock-number-face
       integer, allocatable :: visitors(:)
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^ font-lock-keyword-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^ nil
!                                      ^ f90-ts-font-lock-bracket-face
!                                       ^ f90-ts-font-lock-delimiter-face
!                                        ^ f90-ts-font-lock-bracket-face
  contains
! ^^^^^^^^ font-lock-keyword-face
       procedure :: print => print_result
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^ font-lock-function-name-face
!                         ^^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^^ font-lock-function-name-face
       procedure :: add_visitor
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^ font-lock-function-name-face
       procedure :: compute => compute_sequence
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^ font-lock-function-name-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^^^^^^^^ font-lock-function-name-face
  end type collatz_seq_t
! ^^^ font-lock-keyword-face
!     ^^^^ font-lock-keyword-face
!          ^^^^^^^^^^^^^ font-lock-type-face

  ! Main processor type
! ^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  type :: collatz_processor_t
! ^^^^ font-lock-keyword-face
!      ^^ f90-ts-font-lock-delimiter-face
!         ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
       integer :: n_max
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^ nil
       class(collatz_seq_t), allocatable :: results(:)
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^ font-lock-type-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^ font-lock-keyword-face
!                                        ^^ f90-ts-font-lock-delimiter-face
!                                           ^^^^^^^ nil
!                                                  ^ f90-ts-font-lock-bracket-face
!                                                   ^ f90-ts-font-lock-delimiter-face
!                                                    ^ f90-ts-font-lock-bracket-face
  contains
! ^^^^^^^^ font-lock-keyword-face
       procedure :: compute => compute_all_sequences
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^ font-lock-function-name-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
       procedure :: print_summary
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^ font-lock-function-name-face
       procedure :: find_longest_sequence, &
!      ^^^^^^^^^ font-lock-keyword-face
!                ^^ f90-ts-font-lock-delimiter-face
!                   ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                                        ^ f90-ts-font-lock-delimiter-face
!                                          ^ f90-ts-font-lock-delimiter-face
              find_most_visited
!             ^^^^^^^^^^^^^^^^^ font-lock-function-name-face
  end type collatz_processor_t
! ^^^ font-lock-keyword-face
!     ^^^^ font-lock-keyword-face
!          ^^^^^^^^^^^^^^^^^^^ font-lock-type-face

 contains
!^^^^^^^^ font-lock-keyword-face

  ! Add a visitor to a result
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine add_visitor(self, visitor)
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^ font-lock-function-name-face
!                       ^ f90-ts-font-lock-bracket-face
!                        ^^^^ f90-ts-font-lock-special-var-face
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^ nil
!                                     ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_seq_t), intent(inout) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^ font-lock-type-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^^^ font-lock-keyword-face
!                                          ^^ f90-ts-font-lock-delimiter-face
!                                             ^^^^ f90-ts-font-lock-special-var-face
       integer, intent(in) :: visitor
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^^ nil
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer, allocatable :: tmp(:)
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^ font-lock-keyword-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^ f90-ts-font-lock-delimiter-face
!                                   ^ f90-ts-font-lock-bracket-face
       integer :: n
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       ! TODO: always copy this is O(n**2) and can become expensive
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       n = size(self%visitors)
!      ^ nil
!        ^ f90-ts-font-lock-operator-face
!          ^^^^ font-lock-builtin-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ f90-ts-font-lock-special-var-face
!                   ^ f90-ts-font-lock-operator-face
!                    ^^^^^^^^ nil
!                            ^ f90-ts-font-lock-bracket-face
       allocate(tmp(n + 1))
!      ^^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^ nil
!                  ^ f90-ts-font-lock-bracket-face
!                   ^ nil
!                     ^ f90-ts-font-lock-operator-face
!                       ^ font-lock-number-face
!                        ^^ f90-ts-font-lock-bracket-face
       if (n > 0) &
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^ nil
!            ^ nil
!              ^ font-lock-number-face
!               ^ f90-ts-font-lock-bracket-face
!                 ^ f90-ts-font-lock-delimiter-face
              tmp(1:n) = self%visitors(1:n)
!             ^^^ nil
!                ^ f90-ts-font-lock-bracket-face
!                 ^ font-lock-number-face
!                  ^ f90-ts-font-lock-delimiter-face
!                   ^ nil
!                    ^ f90-ts-font-lock-bracket-face
!                      ^ f90-ts-font-lock-operator-face
!                        ^^^^ f90-ts-font-lock-special-var-face
!                            ^ f90-ts-font-lock-operator-face
!                             ^^^^^^^^ nil
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^ font-lock-number-face
!                                       ^ f90-ts-font-lock-delimiter-face
!                                        ^ nil
!                                         ^ f90-ts-font-lock-bracket-face
       deallocate(self%visitors)
!      ^^^^^^^^^^ font-lock-keyword-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^^^^ f90-ts-font-lock-special-var-face
!                     ^ f90-ts-font-lock-operator-face
!                      ^^^^^^^^ nil
!                              ^ f90-ts-font-lock-bracket-face
       tmp(n + 1) = visitor
!      ^^^ nil
!         ^ f90-ts-font-lock-bracket-face
!          ^ nil
!            ^ f90-ts-font-lock-operator-face
!              ^ font-lock-number-face
!               ^ f90-ts-font-lock-bracket-face
!                 ^ f90-ts-font-lock-operator-face
!                   ^^^^^^^ nil
       call move_alloc(tmp, self%visitors)
!      ^^^^ font-lock-keyword-face
!           ^^^^^^^^^^ font-lock-function-name-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-delimiter-face
!                           ^^^^ f90-ts-font-lock-special-var-face
!                               ^ f90-ts-font-lock-operator-face
!                                ^^^^^^^^ nil
!                                        ^ f90-ts-font-lock-bracket-face
  end subroutine add_visitor
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^ font-lock-function-name-face

  ! Print individual result
! ^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine print_result(self)
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^ font-lock-function-name-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^^^^ f90-ts-font-lock-special-var-face
!                             ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_seq_t), intent(in) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^ font-lock-type-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^ font-lock-keyword-face
!                                       ^^ f90-ts-font-lock-delimiter-face
!                                          ^^^^ f90-ts-font-lock-special-var-face
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: i
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       write(output_unit, '(A,I0)') 'Number: ', self%start_number
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^ font-lock-string-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^^^^^^^^^^ font-lock-string-face
!                                             ^ f90-ts-font-lock-delimiter-face
!                                               ^^^^ f90-ts-font-lock-special-var-face
!                                                   ^ f90-ts-font-lock-operator-face
!                                                    ^^^^^^^^^^^^ nil
       write(output_unit, '(A,I0)') '  Sequence length: ', self%sequence_length
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^ font-lock-string-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                        ^ f90-ts-font-lock-delimiter-face
!                                                          ^^^^ f90-ts-font-lock-special-var-face
!                                                              ^ f90-ts-font-lock-operator-face
!                                                               ^^^^^^^^^^^^^^^ nil
       write(output_unit, '(A,I0)') '  Odd steps (3n+1): ', self%n_odd_steps
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^ font-lock-string-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                         ^ f90-ts-font-lock-delimiter-face
!                                                           ^^^^ f90-ts-font-lock-special-var-face
!                                                               ^ f90-ts-font-lock-operator-face
!                                                                ^^^^^^^^^^^ nil
       write(output_unit, '(A,I0)') '  Maximum value: ', self%max_value
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^ font-lock-string-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                      ^ f90-ts-font-lock-delimiter-face
!                                                        ^^^^ f90-ts-font-lock-special-var-face
!                                                            ^ f90-ts-font-lock-operator-face
!                                                             ^^^^^^^^^ nil
       write(output_unit, '(A,I0,A)') '  Visited by ', size(self%visitors), ' other numbers'
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^ font-lock-string-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^^^^^^^^^^^^ font-lock-string-face
!                                                    ^ f90-ts-font-lock-delimiter-face
!                                                      ^^^^ font-lock-builtin-face
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                           ^^^^ f90-ts-font-lock-special-var-face
!                                                               ^ f90-ts-font-lock-operator-face
!                                                                ^^^^^^^^ nil
!                                                                        ^ f90-ts-font-lock-bracket-face
!                                                                         ^ f90-ts-font-lock-delimiter-face
!                                                                           ^^^^^^^^^^^^^^^^ font-lock-string-face
       if (size(self%visitors) > 0 .and. size(self%visitors) <= 10) then
!      ^^ font-lock-keyword-face
!         ^ f90-ts-font-lock-bracket-face
!          ^^^^ font-lock-builtin-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ f90-ts-font-lock-special-var-face
!                   ^ f90-ts-font-lock-operator-face
!                    ^^^^^^^^ nil
!                            ^ f90-ts-font-lock-bracket-face
!                              ^ nil
!                                ^ font-lock-number-face
!                                  ^^^^^ f90-ts-font-lock-operator-face
!                                        ^^^^ font-lock-builtin-face
!                                            ^ f90-ts-font-lock-bracket-face
!                                             ^^^^ f90-ts-font-lock-special-var-face
!                                                 ^ f90-ts-font-lock-operator-face
!                                                  ^^^^^^^^ nil
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                            ^^ nil
!                                                               ^^ font-lock-number-face
!                                                                 ^ f90-ts-font-lock-bracket-face
!                                                                   ^^^^ font-lock-keyword-face
            write(output_unit, '(A)', advance='no') '    Visitors: '
!           ^^^^^ font-lock-builtin-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^^^^^ font-lock-string-face
!                                   ^ f90-ts-font-lock-delimiter-face
!                                     ^^^^^^^ nil
!                                            ^ f90-ts-font-lock-operator-face
!                                             ^^^^ font-lock-string-face
!                                                 ^ f90-ts-font-lock-bracket-face
!                                                   ^^^^^^^^^^^^^^^^ font-lock-string-face
            do i = 1, size(self%visitors)
!           ^^ font-lock-keyword-face
!              ^ nil
!                ^ f90-ts-font-lock-operator-face
!                  ^ font-lock-number-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^ font-lock-builtin-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^^^^ f90-ts-font-lock-special-var-face
!                              ^ f90-ts-font-lock-operator-face
!                               ^^^^^^^^ nil
!                                       ^ f90-ts-font-lock-bracket-face
                 write(output_unit, '(I0,1X)', advance='no') self%visitors(i)
!                ^^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^^^^^^^^^ nil
!                                 ^ f90-ts-font-lock-delimiter-face
!                                   ^^^^^^^^^ font-lock-string-face
!                                            ^ f90-ts-font-lock-delimiter-face
!                                              ^^^^^^^ nil
!                                                     ^ f90-ts-font-lock-operator-face
!                                                      ^^^^ font-lock-string-face
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                            ^^^^ f90-ts-font-lock-special-var-face
!                                                                ^ f90-ts-font-lock-operator-face
!                                                                 ^^^^^^^^ nil
!                                                                         ^ f90-ts-font-lock-bracket-face
!                                                                          ^ nil
!                                                                           ^ f90-ts-font-lock-bracket-face
            end do
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face
            write(output_unit, *)
!           ^^^^^ font-lock-builtin-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ nil
!                               ^ f90-ts-font-lock-bracket-face
       end if
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end subroutine print_result
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^^ font-lock-function-name-face

  ! Compute Collatz sequence for a single number
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine compute_sequence(self, &
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                            ^ f90-ts-font-lock-bracket-face
!                             ^^^^ f90-ts-font-lock-special-var-face
!                                 ^ f90-ts-font-lock-delimiter-face
!                                   ^ f90-ts-font-lock-delimiter-face
                              start, &
!                             ^^^^^ nil
!                                  ^ f90-ts-font-lock-delimiter-face
!                                    ^ f90-ts-font-lock-delimiter-face
                              seq)
!                             ^^^ nil
!                                ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_seq_t), intent(inout) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^ font-lock-type-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^^^ font-lock-keyword-face
!                                          ^^ f90-ts-font-lock-delimiter-face
!                                             ^^^^ f90-ts-font-lock-special-var-face
       integer, intent(in) :: start
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^^^^^ nil
       integer, allocatable, intent(out) :: seq(:)
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^ font-lock-keyword-face
!                                        ^^ f90-ts-font-lock-delimiter-face
!                                           ^^^ nil
!                                              ^ f90-ts-font-lock-bracket-face
!                                               ^ f90-ts-font-lock-delimiter-face
!                                                ^ f90-ts-font-lock-bracket-face
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: x, size_seq, i
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^ nil
       integer, allocatable :: tmp(:)
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^ font-lock-keyword-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^ f90-ts-font-lock-delimiter-face
!                                   ^ f90-ts-font-lock-bracket-face
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       self%start_number = start
!      ^^^^ f90-ts-font-lock-special-var-face
!          ^ f90-ts-font-lock-operator-face
!           ^^^^^^^^^^^^ nil
!                        ^ f90-ts-font-lock-operator-face
!                          ^^^^^ nil

       size_seq = 100
!      ^^^^^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                 ^^^ font-lock-number-face
       allocate(seq(size_seq))
!      ^^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^ nil
!                  ^ f90-ts-font-lock-bracket-face
!                   ^^^^^^^^ nil
!                           ^^ f90-ts-font-lock-bracket-face

       associate(length => self%sequence_length, &
!      ^^^^^^^^^ font-lock-keyword-face
!               ^ f90-ts-font-lock-bracket-face
!                ^^^^^^ nil
!                       ^^ f90-ts-font-lock-delimiter-face
!                          ^^^^ f90-ts-font-lock-special-var-face
!                              ^ f90-ts-font-lock-operator-face
!                               ^^^^^^^^^^^^^^^ nil
!                                              ^ f90-ts-font-lock-delimiter-face
!                                                ^ f90-ts-font-lock-delimiter-face
                 cnt_odd => self%n_odd_steps)
!                ^^^^^^^ nil
!                        ^^ f90-ts-font-lock-delimiter-face
!                           ^^^^ f90-ts-font-lock-special-var-face
!                               ^ f90-ts-font-lock-operator-face
!                                ^^^^^^^^^^^ nil
!                                           ^ f90-ts-font-lock-bracket-face
            x = start
!           ^ nil
!             ^ f90-ts-font-lock-operator-face
!               ^^^^^ nil
            length = 0
!           ^^^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^ font-lock-number-face
            cnt_odd = 0
!           ^^^^^^^ nil
!                   ^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face

            do
!           ^^ font-lock-keyword-face
                 ! Expand array if needed
!                ^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
                 if (length >= size_seq) then
!                ^^ font-lock-keyword-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^^^^ nil
!                           ^^ nil
!                              ^^^^^^^^ nil
!                                      ^ f90-ts-font-lock-bracket-face
!                                        ^^^^ font-lock-keyword-face
                      size_seq = 2 * size_seq
!                     ^^^^^^^^ nil
!                              ^ f90-ts-font-lock-operator-face
!                                ^ font-lock-number-face
!                                  ^ f90-ts-font-lock-operator-face
!                                    ^^^^^^^^ nil
                      allocate(tmp(size_seq))
!                     ^^^^^^^^ font-lock-keyword-face
!                             ^ f90-ts-font-lock-bracket-face
!                              ^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^^^ nil
!                                          ^^ f90-ts-font-lock-bracket-face
                      tmp(1:length) = seq(1:length)
!                     ^^^ nil
!                        ^ f90-ts-font-lock-bracket-face
!                         ^ font-lock-number-face
!                          ^ f90-ts-font-lock-delimiter-face
!                           ^^^^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^ f90-ts-font-lock-operator-face
!                                     ^^^ nil
!                                        ^ f90-ts-font-lock-bracket-face
!                                         ^ font-lock-number-face
!                                          ^ f90-ts-font-lock-delimiter-face
!                                           ^^^^^^ nil
!                                                 ^ f90-ts-font-lock-bracket-face
                      call move_alloc(tmp, seq)
!                     ^^^^ font-lock-keyword-face
!                          ^^^^^^^^^^ font-lock-function-name-face
!                                    ^ f90-ts-font-lock-bracket-face
!                                     ^^^ nil
!                                        ^ f90-ts-font-lock-delimiter-face
!                                          ^^^ nil
!                                             ^ f90-ts-font-lock-bracket-face
                 end if
!                ^^^ font-lock-keyword-face
!                    ^^ font-lock-keyword-face

                 length = length + 1
!                ^^^^^^ nil
!                       ^ f90-ts-font-lock-operator-face
!                         ^^^^^^ nil
!                                ^ f90-ts-font-lock-operator-face
!                                  ^ font-lock-number-face
                 seq(length) = x
!                ^^^ nil
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^^^^ nil
!                          ^ f90-ts-font-lock-bracket-face
!                            ^ f90-ts-font-lock-operator-face
!                              ^ nil

                 if (x == 1) then
!                ^^ font-lock-keyword-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^ nil
!                      ^^ nil
!                         ^ font-lock-number-face
!                          ^ f90-ts-font-lock-bracket-face
!                            ^^^^ font-lock-keyword-face
                      exit
!                     ^^^^ font-lock-keyword-face
                 else
!                ^^^^ font-lock-keyword-face
                      if (mod(x, 2) == 1) then
!                     ^^ font-lock-keyword-face
!                        ^ f90-ts-font-lock-bracket-face
!                         ^^^ font-lock-builtin-face
!                            ^ f90-ts-font-lock-bracket-face
!                             ^ nil
!                              ^ f90-ts-font-lock-delimiter-face
!                                ^ font-lock-number-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                   ^^ nil
!                                      ^ font-lock-number-face
!                                       ^ f90-ts-font-lock-bracket-face
!                                         ^^^^ font-lock-keyword-face
                           x = 3 * x + 1
!                          ^ nil
!                            ^ f90-ts-font-lock-operator-face
!                              ^ font-lock-number-face
!                                ^ f90-ts-font-lock-operator-face
!                                  ^ nil
!                                    ^ f90-ts-font-lock-operator-face
!                                      ^ font-lock-number-face
                           cnt_odd = cnt_odd + 1
!                          ^^^^^^^ nil
!                                  ^ f90-ts-font-lock-operator-face
!                                    ^^^^^^^ nil
!                                            ^ f90-ts-font-lock-operator-face
!                                              ^ font-lock-number-face
                      else
!                     ^^^^ font-lock-keyword-face
                           x = x / 2
!                          ^ nil
!                            ^ f90-ts-font-lock-operator-face
!                              ^ nil
!                                ^ f90-ts-font-lock-operator-face
!                                  ^ font-lock-number-face
                      end if
!                     ^^^ font-lock-keyword-face
!                         ^^ font-lock-keyword-face
                 end if
!                ^^^ font-lock-keyword-face
!                    ^^ font-lock-keyword-face
            end do
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face
       end associate
!      ^^^ font-lock-keyword-face
!          ^^^^^^^^^ font-lock-keyword-face
  end subroutine compute_sequence
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^^^^^^ font-lock-function-name-face


  ! Constructor function
! ^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  function collatz_processor(n) result(processor)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                           ^ f90-ts-font-lock-bracket-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                               ^^^^^^ font-lock-keyword-face
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^^^^^^^^^ default
!                                               ^ f90-ts-font-lock-bracket-face
  ! result
! ^^^^^^^^ f90-ts-font-lock-separator-comment-face
       type(collatz_processor_t) :: processor
!      ^^^^ font-lock-keyword-face
!          ^ f90-ts-font-lock-bracket-face
!           ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                              ^ f90-ts-font-lock-bracket-face
!                                ^^ f90-ts-font-lock-delimiter-face
!                                   ^^^^^^^^^ nil
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer, intent(in) :: n
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^ nil
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: i
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       processor%n_max = n
!      ^^^^^^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^ nil
!                      ^ f90-ts-font-lock-operator-face
!                        ^ nil
       allocate(collatz_seq_t :: processor%results(1:n))
!      ^^^^^^^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^^^^^^^^ font-lock-type-face
!                             ^^ f90-ts-font-lock-delimiter-face
!                                ^^^^^^^^^ nil
!                                         ^ f90-ts-font-lock-operator-face
!                                          ^^^^^^^ nil
!                                                 ^ f90-ts-font-lock-bracket-face
!                                                  ^ font-lock-number-face
!                                                   ^ f90-ts-font-lock-delimiter-face
!                                                    ^ nil
!                                                     ^^ f90-ts-font-lock-bracket-face
       do i = 1,n
!      ^^ font-lock-keyword-face
!         ^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!              ^ f90-ts-font-lock-delimiter-face
!               ^ nil
            ! Allocate with size 0,
!           ^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            ! Length of visitors array is number of visitors
!           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            allocate(processor%results(i)%visitors(0))
!           ^^^^^^^^ font-lock-keyword-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-operator-face
!                              ^^^^^^^ nil
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^ nil
!                                       ^ f90-ts-font-lock-bracket-face
!                                        ^ f90-ts-font-lock-operator-face
!                                         ^^^^^^^^ nil
!                                                 ^ f90-ts-font-lock-bracket-face
!                                                  ^ font-lock-number-face
!                                                   ^^ f90-ts-font-lock-bracket-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end function collatz_processor
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^^^^^^^^^^^^ font-lock-function-name-face

  ! Compute all sequences and track visitors
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine compute_all_sequences(self)
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^^^^ f90-ts-font-lock-special-var-face
!                                      ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_processor_t), intent(inout) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^^^^ font-lock-keyword-face
!                                                ^^ f90-ts-font-lock-delimiter-face
!                                                   ^^^^ f90-ts-font-lock-special-var-face
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: i, j
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^ nil
       integer, allocatable :: seq(:)
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^^ font-lock-keyword-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^ f90-ts-font-lock-delimiter-face
!                                   ^ f90-ts-font-lock-bracket-face
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       do i = 1, self%n_max
!      ^^ font-lock-keyword-face
!         ^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^ f90-ts-font-lock-special-var-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^^^ nil
            call self%results(i)%compute(i, &
!           ^^^^ font-lock-keyword-face
!                ^^^^ f90-ts-font-lock-special-var-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^^^^^ nil
!                            ^ f90-ts-font-lock-bracket-face
!                             ^ nil
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ f90-ts-font-lock-operator-face
!                                ^^^^^^^ nil
!                                       ^ f90-ts-font-lock-bracket-face
!                                        ^ nil
!                                         ^ f90-ts-font-lock-delimiter-face
!                                           ^ f90-ts-font-lock-delimiter-face
                                         seq &
!                                        ^^^ nil
!                                            ^ f90-ts-font-lock-delimiter-face
                                        )
!                                       ^ f90-ts-font-lock-bracket-face

            self%results(i)%max_value = maxval(seq(1:self%results(i)%sequence_length))
!           ^^^^ f90-ts-font-lock-special-var-face
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ nil
!                         ^ f90-ts-font-lock-bracket-face
!                          ^ f90-ts-font-lock-operator-face
!                           ^^^^^^^^^ nil
!                                     ^ f90-ts-font-lock-operator-face
!                                       ^^^^^^ font-lock-builtin-face
!                                             ^ f90-ts-font-lock-bracket-face
!                                              ^^^ nil
!                                                 ^ f90-ts-font-lock-bracket-face
!                                                  ^ font-lock-number-face
!                                                   ^ f90-ts-font-lock-delimiter-face
!                                                    ^^^^ f90-ts-font-lock-special-var-face
!                                                        ^ f90-ts-font-lock-operator-face
!                                                         ^^^^^^^ nil
!                                                                ^ f90-ts-font-lock-bracket-face
!                                                                 ^ nil
!                                                                  ^ f90-ts-font-lock-bracket-face
!                                                                   ^ f90-ts-font-lock-operator-face
!                                                                    ^^^^^^^^^^^^^^^ nil
!                                                                                   ^^ f90-ts-font-lock-bracket-face

            ! Track visitors: which numbers visit each position
!           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
            do j = 1, self%results(i)%sequence_length
!           ^^ font-lock-keyword-face
!              ^ nil
!                ^ f90-ts-font-lock-operator-face
!                  ^ font-lock-number-face
!                   ^ f90-ts-font-lock-delimiter-face
!                     ^^^^ f90-ts-font-lock-special-var-face
!                         ^ f90-ts-font-lock-operator-face
!                          ^^^^^^^ nil
!                                 ^ f90-ts-font-lock-bracket-face
!                                  ^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^ f90-ts-font-lock-operator-face
!                                     ^^^^^^^^^^^^^^^ nil
                 if (seq(j) <= self%n_max .and. seq(j) /= i) then
!                ^^ font-lock-keyword-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                        ^ nil
!                         ^ f90-ts-font-lock-bracket-face
!                           ^^ nil
!                              ^^^^ f90-ts-font-lock-special-var-face
!                                  ^ f90-ts-font-lock-operator-face
!                                   ^^^^^ nil
!                                         ^^^^^ f90-ts-font-lock-operator-face
!                                               ^^^ nil
!                                                  ^ f90-ts-font-lock-bracket-face
!                                                   ^ nil
!                                                    ^ f90-ts-font-lock-bracket-face
!                                                      ^^ nil
!                                                         ^ nil
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                            ^^^^ font-lock-keyword-face
                      call self%results(seq(j))%add_visitor(i)
!                     ^^^^ font-lock-keyword-face
!                          ^^^^ f90-ts-font-lock-special-var-face
!                              ^ f90-ts-font-lock-operator-face
!                               ^^^^^^^ nil
!                                      ^ f90-ts-font-lock-bracket-face
!                                       ^^^ nil
!                                          ^ f90-ts-font-lock-bracket-face
!                                           ^ nil
!                                            ^^ f90-ts-font-lock-bracket-face
!                                              ^ f90-ts-font-lock-operator-face
!                                               ^^^^^^^^^^^ nil
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                           ^ nil
!                                                            ^ f90-ts-font-lock-bracket-face
                 end if
!                ^^^ font-lock-keyword-face
!                    ^^ font-lock-keyword-face
            end do
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face

            deallocate(seq)
!           ^^^^^^^^^^ font-lock-keyword-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^ nil
!                         ^ f90-ts-font-lock-bracket-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end subroutine compute_all_sequences
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face

  ! Find number with longest sequence
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  function find_longest_sequence(self) result(idx)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^^^^ f90-ts-font-lock-special-var-face
!                                    ^ f90-ts-font-lock-bracket-face
!                                      ^^^^^^ font-lock-keyword-face
!                                            ^ f90-ts-font-lock-bracket-face
!                                             ^^^ default
!                                                ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_processor_t), intent(in) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^ font-lock-keyword-face
!                                             ^^ f90-ts-font-lock-delimiter-face
!                                                ^^^^ f90-ts-font-lock-special-var-face
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: idx, i, max_len
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       max_len = 0
!      ^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^ font-lock-number-face
       idx = 1
!      ^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^ font-lock-number-face
       do i = 1, self%n_max
!      ^^ font-lock-keyword-face
!         ^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^ f90-ts-font-lock-special-var-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^^^ nil
            if (self%results(i)%sequence_length > max_len) then
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^ f90-ts-font-lock-special-var-face
!                   ^ f90-ts-font-lock-operator-face
!                    ^^^^^^^ nil
!                           ^ f90-ts-font-lock-bracket-face
!                            ^ nil
!                             ^ f90-ts-font-lock-bracket-face
!                              ^ f90-ts-font-lock-operator-face
!                               ^^^^^^^^^^^^^^^ nil
!                                               ^ nil
!                                                 ^^^^^^^ nil
!                                                        ^ f90-ts-font-lock-bracket-face
!                                                          ^^^^ font-lock-keyword-face
                 max_len = self%results(i)%sequence_length
!                ^^^^^^^ nil
!                        ^ f90-ts-font-lock-operator-face
!                          ^^^^ f90-ts-font-lock-special-var-face
!                              ^ f90-ts-font-lock-operator-face
!                               ^^^^^^^ nil
!                                      ^ f90-ts-font-lock-bracket-face
!                                       ^ nil
!                                        ^ f90-ts-font-lock-bracket-face
!                                         ^ f90-ts-font-lock-operator-face
!                                          ^^^^^^^^^^^^^^^ nil
                 idx = i
!                ^^^ nil
!                    ^ f90-ts-font-lock-operator-face
!                      ^ nil
            end if
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end function find_longest_sequence
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^^^^^^^^^^^^^^^^ font-lock-function-name-face

  ! Find most visited number, with less at most max_visits
! ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  function find_most_visited(self, max_visits) result(idx)
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^^^^^^^^^^^^^^ font-lock-function-name-face
!                           ^ f90-ts-font-lock-bracket-face
!                            ^^^^ f90-ts-font-lock-special-var-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^ nil
!                                            ^ f90-ts-font-lock-bracket-face
!                                              ^^^^^^ font-lock-keyword-face
!                                                    ^ f90-ts-font-lock-bracket-face
!                                                     ^^^ default
!                                                        ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_processor_t), intent(in) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^ font-lock-keyword-face
!                                             ^^ f90-ts-font-lock-delimiter-face
!                                                ^^^^ f90-ts-font-lock-special-var-face
       integer, intent(in) :: max_visits
!      ^^^^^^^ font-lock-type-face
!             ^ f90-ts-font-lock-delimiter-face
!               ^^^^^^^^^^ font-lock-keyword-face
!                          ^^ f90-ts-font-lock-delimiter-face
!                             ^^^^^^^^^^ nil
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: idx, i, max_visitors, cnt
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^^^ nil
!                                     ^ f90-ts-font-lock-delimiter-face
!                                       ^^^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       max_visitors = 0
!      ^^^^^^^^^^^^ nil
!                   ^ f90-ts-font-lock-operator-face
!                     ^ font-lock-number-face
       idx = 0
!      ^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^ font-lock-number-face
       do i = 1, self%n_max
!      ^^ font-lock-keyword-face
!         ^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^ font-lock-number-face
!              ^ f90-ts-font-lock-delimiter-face
!                ^^^^ f90-ts-font-lock-special-var-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^^^ nil
            cnt = size(self%results(i)%visitors)
!           ^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                 ^^^^ font-lock-builtin-face
!                     ^ f90-ts-font-lock-bracket-face
!                      ^^^^ f90-ts-font-lock-special-var-face
!                          ^ f90-ts-font-lock-operator-face
!                           ^^^^^^^ nil
!                                  ^ f90-ts-font-lock-bracket-face
!                                   ^ nil
!                                    ^ f90-ts-font-lock-bracket-face
!                                     ^ f90-ts-font-lock-operator-face
!                                      ^^^^^^^^ nil
!                                              ^ f90-ts-font-lock-bracket-face
            if ((max_visitors < cnt) .and. (cnt <= max_visits)) then
!           ^^ font-lock-keyword-face
!              ^^ f90-ts-font-lock-bracket-face
!                ^^^^^^^^^^^^ nil
!                             ^ nil
!                               ^^^ nil
!                                  ^ f90-ts-font-lock-bracket-face
!                                    ^^^^^ f90-ts-font-lock-operator-face
!                                          ^ f90-ts-font-lock-bracket-face
!                                           ^^^ nil
!                                               ^^ nil
!                                                  ^^^^^^^^^^ nil
!                                                            ^^ f90-ts-font-lock-bracket-face
!                                                               ^^^^ font-lock-keyword-face
                 max_visitors = size(self%results(i)%visitors)
!                ^^^^^^^^^^^^ nil
!                             ^ f90-ts-font-lock-operator-face
!                               ^^^^ font-lock-builtin-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^^^^ f90-ts-font-lock-special-var-face
!                                        ^ f90-ts-font-lock-operator-face
!                                         ^^^^^^^ nil
!                                                ^ f90-ts-font-lock-bracket-face
!                                                 ^ nil
!                                                  ^ f90-ts-font-lock-bracket-face
!                                                   ^ f90-ts-font-lock-operator-face
!                                                    ^^^^^^^^ nil
!                                                            ^ f90-ts-font-lock-bracket-face
                 idx = i
!                ^^^ nil
!                    ^ f90-ts-font-lock-operator-face
!                      ^ nil
            end if
!           ^^^ font-lock-keyword-face
!               ^^ font-lock-keyword-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face
  end function find_most_visited
! ^^^ font-lock-keyword-face
!     ^^^^^^^^ font-lock-keyword-face
!              ^^^^^^^^^^^^^^^^^ font-lock-function-name-face

  ! Print summary statistics
! ^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
  subroutine print_summary(self)
! ^^^^^^^^^^ font-lock-keyword-face
!            ^^^^^^^^^^^^^ font-lock-function-name-face
!                         ^ f90-ts-font-lock-bracket-face
!                          ^^^^ f90-ts-font-lock-special-var-face
!                              ^ f90-ts-font-lock-bracket-face
  ! arguments
! ^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       class(collatz_processor_t), intent(in) :: self
!      ^^^^^ font-lock-keyword-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                               ^ f90-ts-font-lock-bracket-face
!                                ^ f90-ts-font-lock-delimiter-face
!                                  ^^^^^^^^^^ font-lock-keyword-face
!                                             ^^ f90-ts-font-lock-delimiter-face
!                                                ^^^^ f90-ts-font-lock-special-var-face
  ! local
! ^^^^^^^ f90-ts-font-lock-separator-comment-face
       integer :: longest_idx, most_visited_idx, max_visits
!      ^^^^^^^ font-lock-type-face
!              ^^ f90-ts-font-lock-delimiter-face
!                 ^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^^^^^^^^ nil
!                                              ^ f90-ts-font-lock-delimiter-face
!                                                ^^^^^^^^^^ nil
       real :: avg_length, avg_odd
!      ^^^^ font-lock-type-face
!           ^^ f90-ts-font-lock-delimiter-face
!              ^^^^^^^^^^ nil
!                        ^ f90-ts-font-lock-delimiter-face
!                          ^^^^^^^ nil
  ! ==========
! ^^^^^^^^^^^^ f90-ts-font-lock-separator-comment-face
       write(output_unit, '(/,A)') repeat('=', 60)
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^ font-lock-string-face
!                                ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^ font-lock-builtin-face
!                                        ^ f90-ts-font-lock-bracket-face
!                                         ^^^ font-lock-string-face
!                                            ^ f90-ts-font-lock-delimiter-face
!                                              ^^ font-lock-number-face
!                                                ^ f90-ts-font-lock-bracket-face
       write(output_unit, '(A,I0,A)') 'COLLATZ SEQUENCE ANALYSIS (N = 1 to ', self%n_max, ')'
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^ font-lock-string-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                                           ^ f90-ts-font-lock-delimiter-face
!                                                                             ^^^^ f90-ts-font-lock-special-var-face
!                                                                                 ^ f90-ts-font-lock-operator-face
!                                                                                  ^^^^^ nil
!                                                                                       ^ f90-ts-font-lock-delimiter-face
!                                                                                         ^^^ font-lock-string-face
       write(output_unit, '(A)') repeat('=', 60)
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^ font-lock-string-face
!                              ^ f90-ts-font-lock-bracket-face
!                                ^^^^^^ font-lock-builtin-face
!                                      ^ f90-ts-font-lock-bracket-face
!                                       ^^^ font-lock-string-face
!                                          ^ f90-ts-font-lock-delimiter-face
!                                            ^^ font-lock-number-face
!                                              ^ f90-ts-font-lock-bracket-face

       ! Calculate averages
!      ^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       avg_length = sum(self%results(:)%sequence_length) / real(self%n_max)
!      ^^^^^^^^^^ nil
!                 ^ f90-ts-font-lock-operator-face
!                   ^^^ font-lock-builtin-face
!                      ^ f90-ts-font-lock-bracket-face
!                       ^^^^ f90-ts-font-lock-special-var-face
!                           ^ f90-ts-font-lock-operator-face
!                            ^^^^^^^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^ f90-ts-font-lock-delimiter-face
!                                     ^ f90-ts-font-lock-bracket-face
!                                      ^ f90-ts-font-lock-operator-face
!                                       ^^^^^^^^^^^^^^^ nil
!                                                      ^ f90-ts-font-lock-bracket-face
!                                                        ^ f90-ts-font-lock-operator-face
!                                                          ^^^^ font-lock-builtin-face
!                                                              ^ f90-ts-font-lock-bracket-face
!                                                               ^^^^ f90-ts-font-lock-special-var-face
!                                                                   ^ f90-ts-font-lock-operator-face
!                                                                    ^^^^^ nil
!                                                                         ^ f90-ts-font-lock-bracket-face
       avg_odd = sum(self%results(:)%n_odd_steps) / real(self%n_max)
!      ^^^^^^^ nil
!              ^ f90-ts-font-lock-operator-face
!                ^^^ font-lock-builtin-face
!                   ^ f90-ts-font-lock-bracket-face
!                    ^^^^ f90-ts-font-lock-special-var-face
!                        ^ f90-ts-font-lock-operator-face
!                         ^^^^^^^ nil
!                                ^ f90-ts-font-lock-bracket-face
!                                 ^ f90-ts-font-lock-delimiter-face
!                                  ^ f90-ts-font-lock-bracket-face
!                                   ^ f90-ts-font-lock-operator-face
!                                    ^^^^^^^^^^^ nil
!                                               ^ f90-ts-font-lock-bracket-face
!                                                 ^ f90-ts-font-lock-operator-face
!                                                   ^^^^ font-lock-builtin-face
!                                                       ^ f90-ts-font-lock-bracket-face
!                                                        ^^^^ f90-ts-font-lock-special-var-face
!                                                            ^ f90-ts-font-lock-operator-face
!                                                             ^^^^^ nil
!                                                                  ^ f90-ts-font-lock-bracket-face

       write(output_unit, '(/,A)') 'Summary Statistics:'
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^ font-lock-string-face
!                                ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
       write(output_unit, '(A,F8.2)') '  Average sequence length: ', avg_length
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^ font-lock-string-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                                  ^ f90-ts-font-lock-delimiter-face
!                                                                    ^^^^^^^^^^ nil
       write(output_unit, '(A,F8.2)') '  Average odd steps (3n+1): ', avg_odd
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^^^^ font-lock-string-face
!                                   ^ f90-ts-font-lock-bracket-face
!                                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                                   ^ f90-ts-font-lock-delimiter-face
!                                                                     ^^^^^^^ nil

       ! Longest sequence
!      ^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       longest_idx = self%find_longest_sequence()
!      ^^^^^^^^^^^ nil
!                  ^ f90-ts-font-lock-operator-face
!                    ^^^^ f90-ts-font-lock-special-var-face
!                        ^ f90-ts-font-lock-operator-face
!                         ^^^^^^^^^^^^^^^^^^^^^ nil
!                                              ^^ f90-ts-font-lock-bracket-face
       write(output_unit, '(/,A)') 'Longest Sequence:'
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^ font-lock-string-face
!                                ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^^^^^^^^^^^^^^ font-lock-string-face
       call self%results(longest_idx)%print()
!      ^^^^ font-lock-keyword-face
!           ^^^^ f90-ts-font-lock-special-var-face
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^^ nil
!                       ^ f90-ts-font-lock-bracket-face
!                        ^^^^^^^^^^^ nil
!                                   ^ f90-ts-font-lock-bracket-face
!                                    ^ f90-ts-font-lock-operator-face
!                                     ^^^^^ nil
!                                          ^^ f90-ts-font-lock-bracket-face

       ! Most visited (first result must be idx=1)
!      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
       max_visits = self%n_max
!      ^^^^^^^^^^ nil
!                 ^ f90-ts-font-lock-operator-face
!                   ^^^^ f90-ts-font-lock-special-var-face
!                       ^ f90-ts-font-lock-operator-face
!                        ^^^^^ nil
       do
!      ^^ font-lock-keyword-face
            most_visited_idx = self%find_most_visited(max_visits)
!           ^^^^^^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-operator-face
!                              ^^^^ f90-ts-font-lock-special-var-face
!                                  ^ f90-ts-font-lock-operator-face
!                                   ^^^^^^^^^^^^^^^^^ nil
!                                                    ^ f90-ts-font-lock-bracket-face
!                                                     ^^^^^^^^^^ nil
!                                                               ^ f90-ts-font-lock-bracket-face
            if (most_visited_idx == 0) &
!           ^^ font-lock-keyword-face
!              ^ f90-ts-font-lock-bracket-face
!               ^^^^^^^^^^^^^^^^ nil
!                                ^^ nil
!                                   ^ font-lock-number-face
!                                    ^ f90-ts-font-lock-bracket-face
!                                      ^ f90-ts-font-lock-delimiter-face
                   exit
!                  ^^^^ font-lock-keyword-face
            write(output_unit, '(/,A,I0,A)') 'Most visited number with at most ', max_visits, ' visits:'
!           ^^^^^ font-lock-builtin-face
!                ^ f90-ts-font-lock-bracket-face
!                 ^^^^^^^^^^^ nil
!                            ^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^^^^ font-lock-string-face
!                                          ^ f90-ts-font-lock-bracket-face
!                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
!                                                                               ^ f90-ts-font-lock-delimiter-face
!                                                                                 ^^^^^^^^^^ nil
!                                                                                           ^ f90-ts-font-lock-delimiter-face
!                                                                                             ^^^^^^^^^^ font-lock-string-face
            call self%results(most_visited_idx)%print()
!           ^^^^ font-lock-keyword-face
!                ^^^^ f90-ts-font-lock-special-var-face
!                    ^ f90-ts-font-lock-operator-face
!                     ^^^^^^^ nil
!                            ^ f90-ts-font-lock-bracket-face
!                             ^^^^^^^^^^^^^^^^ nil
!                                             ^ f90-ts-font-lock-bracket-face
!                                              ^ f90-ts-font-lock-operator-face
!                                               ^^^^^ nil
!                                                    ^^ f90-ts-font-lock-bracket-face
            max_visits = size(self%results(most_visited_idx)%visitors) / 2
!           ^^^^^^^^^^ nil
!                      ^ f90-ts-font-lock-operator-face
!                        ^^^^ font-lock-builtin-face
!                            ^ f90-ts-font-lock-bracket-face
!                             ^^^^ f90-ts-font-lock-special-var-face
!                                 ^ f90-ts-font-lock-operator-face
!                                  ^^^^^^^ nil
!                                         ^ f90-ts-font-lock-bracket-face
!                                          ^^^^^^^^^^^^^^^^ nil
!                                                          ^ f90-ts-font-lock-bracket-face
!                                                           ^ f90-ts-font-lock-operator-face
!                                                            ^^^^^^^^ nil
!                                                                    ^ f90-ts-font-lock-bracket-face
!                                                                      ^ f90-ts-font-lock-operator-face
!                                                                        ^ font-lock-number-face
       end do
!      ^^^ font-lock-keyword-face
!          ^^ font-lock-keyword-face

       write(output_unit, '(/,A)') repeat('=', 60)
!      ^^^^^ font-lock-builtin-face
!           ^ f90-ts-font-lock-bracket-face
!            ^^^^^^^^^^^ nil
!                       ^ f90-ts-font-lock-delimiter-face
!                         ^^^^^^^ font-lock-string-face
!                                ^ f90-ts-font-lock-bracket-face
!                                  ^^^^^^ font-lock-builtin-face
!                                        ^ f90-ts-font-lock-bracket-face
!                                         ^^^ font-lock-string-face
!                                            ^ f90-ts-font-lock-delimiter-face
!                                              ^^ font-lock-number-face
!                                                ^ f90-ts-font-lock-bracket-face
  end subroutine print_summary
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^ font-lock-keyword-face
!                ^^^^^^^^^^^^^ font-lock-function-name-face

 end module collatz_mod
!^^^ font-lock-keyword-face
!    ^^^^^^ font-lock-keyword-face
!           ^^^^^^^^^^^ font-lock-function-name-face

 program collatz
!^^^^^^^ font-lock-keyword-face
!        ^^^^^^^ font-lock-function-name-face
  use iso_fortran_env, only: output_unit, input_unit
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^^^^^ nil
!                    ^ f90-ts-font-lock-delimiter-face
!                      ^^^^ font-lock-keyword-face
!                          ^ f90-ts-font-lock-delimiter-face
!                            ^^^^^^^^^^^ nil
!                                       ^ f90-ts-font-lock-delimiter-face
!                                         ^^^^^^^^^^ nil
  use collatz_mod
! ^^^ font-lock-keyword-face
!     ^^^^^^^^^^^ nil
  implicit none
! ^^^^^^^^ font-lock-keyword-face
!          ^^^^ nil

  type(collatz_processor_t) :: processor
! ^^^^ font-lock-keyword-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^^^^^^^^^^^ font-lock-type-face
!                         ^ f90-ts-font-lock-bracket-face
!                           ^^ f90-ts-font-lock-delimiter-face
!                              ^^^^^^^^^ nil
  integer :: n
! ^^^^^^^ font-lock-type-face
!         ^^ f90-ts-font-lock-delimiter-face
!            ^ nil

  write(output_unit, *) 'Enter maximum number:'
! ^^^^^ font-lock-builtin-face
!      ^ f90-ts-font-lock-bracket-face
!       ^^^^^^^^^^^ nil
!                  ^ f90-ts-font-lock-delimiter-face
!                    ^ nil
!                     ^ f90-ts-font-lock-bracket-face
!                       ^^^^^^^^^^^^^^^^^^^^^^^ font-lock-string-face
  read(input_unit, *) n
! ^^^^ font-lock-builtin-face
!     ^ f90-ts-font-lock-bracket-face
!      ^^^^^^^^^^ nil
!                ^ f90-ts-font-lock-delimiter-face
!                  ^ nil
!                   ^ f90-ts-font-lock-bracket-face
!                     ^ nil

  processor = collatz_processor(n)
! ^^^^^^^^^ nil
!           ^ f90-ts-font-lock-operator-face
!             ^^^^^^^^^^^^^^^^^ nil
!                              ^ f90-ts-font-lock-bracket-face
!                               ^ nil
!                                ^ f90-ts-font-lock-bracket-face

  call processor%compute()
! ^^^^ font-lock-keyword-face
!      ^^^^^^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^^ nil
!                       ^^ f90-ts-font-lock-bracket-face
  call processor%print_summary()
! ^^^^ font-lock-keyword-face
!      ^^^^^^^^^ nil
!               ^ f90-ts-font-lock-operator-face
!                ^^^^^^^^^^^^^ nil
!                             ^^ f90-ts-font-lock-bracket-face

 end program collatz
!^^^ font-lock-keyword-face
!    ^^^^^^^ font-lock-keyword-face
!            ^^^^^^^ font-lock-function-name-face
