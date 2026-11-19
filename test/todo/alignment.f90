! I/O control lists (open, close, inquire, read, write).
! The control items are flat children of the statement, no argument_list.
subroutine io_control_lists(unit, name, x, n)
   implicit none
   integer, intent(in) :: unit, n
   character(len=*), intent(in) :: name
   real, intent(inout) :: x(n)
   integer :: ios, size_bytes
   logical :: exists
   character(len=80) :: msg

   open(unit=unit, file=name, status='old', action='read', &
        form='formatted', iostat=ios, iomsg=msg)
   inquire(file=name, exist=exists, size=size_bytes, &
           iostat=ios)
   read(unit, *, iostat=ios, &
        iomsg=msg) x
   write(unit, '(a, i0)', iostat=ios, &
         advance='no') 'n = ', n

   ! control list and item list in the same statement; the item list alone
   ! is already supported (output_item_list)
   write(unit, '(2(a, f8.3))') 'first = ', x(1), &
                               'last = ', x(n)
   close(unit, iostat=ios, &
         status='keep')
end subroutine io_control_lists


! allocate, deallocate and nullify.
! Allocations and stat=/errmsg= are flat children inside the parentheses.
subroutine allocation(n, m, ierr)
   implicit none
   integer, intent(in) :: n, m
   integer, intent(out) :: ierr
   real, allocatable :: a(:), b(:, :)
   real, pointer :: p(:), q(:)
   class(*), allocatable :: c(:), d(:)
   character(len=128) :: msg

   allocate(a(n), b(n, m), &
            stat=ierr, errmsg=msg)
   ! typed allocation: items are those after "::"
   allocate(real :: c(n), d(m), &
                    stat=ierr)
   deallocate(a, b, &
              stat=ierr)
   nullify(p, &
           q)
end subroutine allocation


! entity lists without own context: attribute statements
! (variable_modification), enumerators and the legacy statements common,
! namelist, data, equivalence and parameter.
subroutine entity_statements(a, b, c, d)
   implicit none
   real :: a(3), b(3), c(3), d(3)
   real :: t1, t2, t3
   real :: s1, s2, s3
   real :: w1, w2, w3
   real :: v1, v2, v3
   integer :: i1, i2, i3
   real :: d1, d2, d3
   real :: q1, q2, r1, r2
   integer :: k1, k2, k3

   intent(in) :: a, b, &
                 c
   intent(out) :: d
   target :: t1, t2, &
             t3
   save :: s1, s2, &
           s3
   volatile :: w1, w2, &
               w3

   enum, bind(c)
      enumerator :: red = 0, green, blue, &
                    cyan
   end enum

   common /state/ v1, v2, &
                  v3
   namelist /params/ i1, i2, &
                     i3
   data d1 /1.0/, d2 /2.0/, &
        d3 /3.0/
   equivalence (q1, q2), &
               (r1, r2)
   parameter (k1 = 1, k2 = 2, &
              k3 = 3)
end subroutine entity_statements


! do loop control and implied do loops.
! do: items after "=" are aligned (as for concurrent_control and triplet_spec).
! implied do: items inside the parentheses (loop_control_expression).
subroutine loop_control(n, u, a, s)
   implicit none
   integer, intent(in) :: n, u
   real, intent(inout) :: a(n)
   real, intent(out) :: s
   integer :: i

   s = 0.0
   do i = 1, &
          n, &
          2
      s = s + a(i)
   end do
   a(1:n) = [(real(i), &
              i = 1, n)]
   write(u, *) (a(i), &
                i = 1, n)
end subroutine loop_control

! language binding and derived type parameter lists.
! Both are parenthesized lists: items aligned after the opening parenthesis.
module binding_and_type_parameters
   use, intrinsic :: iso_c_binding, only: c_double
   implicit none

   type :: field_t(k, nx, &
                   ny)
      integer, kind :: k = c_double
      integer, len :: nx, ny
      real(k) :: values(nx, ny)
   end type field_t

contains

   subroutine c_scale(x) bind(c, &
                              name='c_scale')
      real(c_double), intent(inout) :: x

      x = 2.0_c_double * x
   end subroutine c_scale

   function c_square(x) result(y) bind(c, &
                                       name='c_square')
      real(c_double), intent(in) :: x
      real(c_double) :: y

      y = x * x
   end function c_square

end module binding_and_type_parameters
