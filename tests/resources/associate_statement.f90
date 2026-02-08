subroutine sub()

     associate(x => y)
     end associate

     associate(expr => sqrt(p**2/4.0 - q), &
               result => arr(5))
          points: associate(p &
                                    => self%points_active, &
                            q &
                                    => self%points_inactive &
                           )
               ! some output
               print *,p%count, q%count
          end associate points
          ! more output
          print *, expr, result
          ! done
     end associate

     components: associate(matrix_A => this%model_A%mat, &
                           matrix_B => this%model_B%mat, &
                           N => this%model_size)
          outer:do i = 1,N
               inner:do j = 1,N
                    matrix_R(i,j) = matrix_A(i,j) &
                           - matrix_B(i,j)
               end do inner
          end do outer
     end associate components

     associate(test => 1+2, &

               after_empty_line => 3+4)
     end associate

     associate(test => 1+2, &
                          ! comment
               after_comment_line => 3+4)
     end associate

     associate(test => 1+2, &

                    ! comment

               after_empty_and_comment_lines => 3+4)
     end associate

end subroutine sub
