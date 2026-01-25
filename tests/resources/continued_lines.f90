function fun(x1, x2, x3, x4, x5)  &
       result( &
       ! this is my result

       something &
       )
     logical :: something
! arguments
     integer &
      :: x1, &
         x2, &
         ! write some comment here
         x3

! ==========
     call foo(x,& ! first arg

              y, & ! second arg

                  ! comment

              z,&
              u,  & ! fourth arg
              v
             )

     x &


            = &
            y &  ! comment 1
            + fun&! comment 2
            ( &

        )

   write(*,format_string_for_testing) &       !comment
        ! comment
        val1, val2, val3, & ! comment

        ! some more comment


        ! comment

        final_val

end function fun
