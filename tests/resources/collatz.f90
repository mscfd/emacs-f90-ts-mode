module collatz_mod
 use iso_fortran_env, only: output_unit
 implicit none
 private

 ! Public interfaces
 public :: collatz_processor, &
        collatz_processor_t

 ! Type for storing results for a single number
 type :: collatz_seq_t
      integer :: start_number = 0
      integer :: sequence_length = 0

      !> number of 3n+1 applications
      integer :: n_odd_steps = 0
      integer :: max_value = 0
      integer, allocatable :: visitors(:)
 contains
      procedure :: print => print_result
      procedure :: add_visitor
      procedure :: compute => compute_sequence
 end type collatz_seq_t

 ! Main processor type
 type :: collatz_processor_t
      integer :: n_max
      class(collatz_seq_t), allocatable :: results(:)
 contains
      procedure :: compute => compute_all_sequences
      procedure :: print_summary
      procedure :: find_longest_sequence, &
             find_most_visited
 end type collatz_processor_t

contains

 ! Add a visitor to a result
 subroutine add_visitor(self, visitor)
      ! arguments
      class(collatz_seq_t), intent(inout) :: self
      integer, intent(in) :: visitor
      ! local
      integer, allocatable :: tmp(:)
      integer :: n
      ! ==========
      ! TODO: always copy this is O(n^2) and can become expensive
      n = size(self%visitors)
      allocate(tmp(n + 1))
      if (n > 0) &
           tmp(1:n) = self%visitors(1:n)
      deallocate(self%visitors)
      tmp(n + 1) = visitor
      call move_alloc(tmp, self%visitors)
 end subroutine add_visitor

 ! Print individual result
 subroutine print_result(self)
      ! arguments
      class(collatz_seq_t), intent(in) :: self
      ! local
      integer :: i
      ! ==========
      write(output_unit, '(A,I0)') 'Number: ', self%start_number
      write(output_unit, '(A,I0)') '  Sequence length: ', self%sequence_length
      write(output_unit, '(A,I0)') '  Odd steps (3n+1): ', self%n_odd_steps
      write(output_unit, '(A,I0)') '  Maximum value: ', self%max_value
      write(output_unit, '(A,I0,A)') '  Visited by ', size(self%visitors), ' other numbers'
      if (size(self%visitors) > 0 .and. size(self%visitors) <= 10) then
           write(output_unit, '(A)', advance='no') '    Visitors: '
           do i = 1, size(self%visitors)
                write(output_unit, '(I0,1X)', advance='no') self%visitors(i)
           end do
           write(output_unit, *)
      end if
 end subroutine print_result

 ! Compute Collatz sequence for a single number
 subroutine compute_sequence(self, &
                             start, &
                             seq)
      ! arguments
      class(collatz_seq_t), intent(inout) :: self
      integer, intent(in) :: start
      integer, allocatable, intent(out) :: seq(:)
      ! local
      integer :: x, size_seq, i
      integer, allocatable :: tmp(:)
      ! ==========
      self%start_number = start

      size_seq = 100
      allocate(seq(size_seq))

      associate(length => self%sequence_length, &
                cnt_odd => self%n_odd_steps)
           x = start
           length = 0
           cnt_odd = 0

           do
                ! Expand array if needed
                if (length >= size_seq) then
                     size_seq = 2 * size_seq
                     allocate(tmp(size_seq))
                     tmp(1:length) = seq(1:length)
                     call move_alloc(tmp, seq)
                end if

                length = length + 1
                seq(length) = x

                if (x == 1) then
                     exit
                else
                     if (mod(x, 2) == 1) then
                          x = 3 * x + 1
                          cnt_odd = cnt_odd + 1
                     else
                          x = x / 2
                     end if
                end if
           end do
      end associate
 end subroutine compute_sequence


 ! Constructor function
 function collatz_processor(n) result(processor)
      ! result
      type(collatz_processor_t) :: processor
      ! arguments
      integer, intent(in) :: n
      ! local
      integer :: i
      ! ==========
      processor%n_max = n
      allocate(collatz_seq_t :: processor%results(1:n))
      do i = 1,n
           ! Allocate with size 0,
           ! Length of visitors array is number of visitors
           allocate(processor%results(i)%visitors(0))
      end do
 end function collatz_processor

 ! Compute all sequences and track visitors
 subroutine compute_all_sequences(self)
      ! arguments
      class(collatz_processor_t), intent(inout) :: self
      ! local
      integer :: i, j
      integer, allocatable :: seq(:)
      ! ==========
      do i = 1, self%n_max
           call self%results(i)%compute(i, &
                                        seq &
                                       )

           self%results(i)%max_value = maxval(seq(1:self%results(i)%sequence_length))

           ! Track visitors: which numbers visit each position
           do j = 1, self%results(i)%sequence_length
                if (seq(j) <= self%n_max .and. seq(j) /= i) then
                     call self%results(seq(j))%add_visitor(i)
                end if
           end do

           deallocate(seq)
      end do
 end subroutine compute_all_sequences

 ! Find number with longest sequence
 function find_longest_sequence(self) result(idx)
      ! arguments
      class(collatz_processor_t), intent(in) :: self
      ! local
      integer :: idx, i, max_len
      ! ==========
      max_len = 0
      idx = 1
      do i = 1, self%n_max
           if (self%results(i)%sequence_length > max_len) then
                max_len = self%results(i)%sequence_length
                idx = i
           end if
      end do
 end function find_longest_sequence

 ! Find most visited number, with less at most max_visits
 function find_most_visited(self, max_visits) result(idx)
      ! arguments
      class(collatz_processor_t), intent(in) :: self
      integer, intent(in) :: max_visits
      ! local
      integer :: idx, i, max_visitors, cnt
      ! ==========
      max_visitors = 0
      idx = 0
      do i = 1, self%n_max
           cnt = size(self%results(i)%visitors)
           if ((max_visitors < cnt) .and. (cnt <= max_visits)) then
                max_visitors = size(self%results(i)%visitors)
                idx = i
           end if
      end do
 end function find_most_visited

 ! Print summary statistics
 subroutine print_summary(self)
      ! arguments
      class(collatz_processor_t), intent(in) :: self
      ! local
      integer :: longest_idx, most_visited_idx, max_visits
      real :: avg_length, avg_odd
      ! ==========
      write(output_unit, '(/,A)') repeat('=', 60)
      write(output_unit, '(A,I0,A)') 'COLLATZ SEQUENCE ANALYSIS (N = 1 to ', self%n_max, ')'
      write(output_unit, '(A)') repeat('=', 60)

      ! Calculate averages
      avg_length = sum(self%results(:)%sequence_length) / real(self%n_max)
      avg_odd = sum(self%results(:)%n_odd_steps) / real(self%n_max)

      write(output_unit, '(/,A)') 'Summary Statistics:'
      write(output_unit, '(A,F8.2)') '  Average sequence length: ', avg_length
      write(output_unit, '(A,F8.2)') '  Average odd steps (3n+1): ', avg_odd

      ! Longest sequence
      longest_idx = self%find_longest_sequence()
      write(output_unit, '(/,A)') 'Longest Sequence:'
      call self%results(longest_idx)%print()

      ! Most visited (first result must be idx=1)
      max_visits = self%n_max
      do
           most_visited_idx = self%find_most_visited(max_visits)
           if (most_visited_idx == 0) &
                  exit
           write(output_unit, '(/,A,I0,A)') 'Most visited number with at most ', max_visits, ' visits:'
           call self%results(most_visited_idx)%print()
           max_visits = size(self%results(most_visited_idx)%visitors) / 2
      end do

      write(output_unit, '(/,A)') repeat('=', 60)
 end subroutine print_summary

end module collatz_mod

! Example program
program collatz
 use iso_fortran_env, only: output_unit, input_unit
 use collatz_mod
 implicit none

 type(collatz_processor_t) :: processor
 integer :: n

 write(output_unit, *) 'Enter maximum number:'
 read(input_unit, *) n

 processor = collatz_processor(n)

 call processor%compute()
 call processor%print_summary()

end program collatz
