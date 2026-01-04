function fun_result(x, &
                    y) result(r)
end function fun_result


real function fun_type(x, &
                       y)
end function fun_type

subroutine sub_simple(x, y, z, &
                      a, &
                      ! comment
                            b)
end subroutine sub_simple

module subroutine sub_mod(x, &
       y)
end subroutine sub_mod


! some fancy continuations
function fun(x1, x2, x3, &
                 x4, x5)&
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
              v&
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

     call algorithm(fun1(a, b), fun2(x, y, &
              z, &
                                   ), &
                            fun3(u, &
            v, &
              w))


end function fun
