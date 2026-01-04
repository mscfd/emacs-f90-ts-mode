module my_mod

 implicit none
 private

 public do_fun

contains

 integer function do_fun()
     do
          print *, "Hello ERT, testing me?"
     end do

     with_exit_: do
          if (i > 10) exit
          i = i + 1
     end do with_exit_

     do while (val < 1.0)
          val = update(val)
     end do

     nested_1: do while (flag .eqv. .true.)
          flag = .false.
          some_if: if (cond) then
               nested_2: do while (k < 5)
                    k = k - 1
               end do nested_2
          end if some_if
          flag = check(k)
     end do nested_1

     sum = 0
     do i = 1,10
          inner: do j = 1,10
               sum = sum + i*j
          end do inner
     end do inner

     label_A: do while (A)
          label_B: do B = 100,1,-3
               label_C: do
                    if (cond(A,B,C) > 30) exit
               end do
          end do
     end do

     do while (A)
          do B = 100,1,-3
               do
                    ! comment
                    if (cond(A,B,C) > 30) exit
               end do
          end do
     end do
 end function do_fun

end module my_mod
