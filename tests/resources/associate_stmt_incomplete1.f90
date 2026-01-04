subroutine sub()

     components: associate(matrix_A => this%model_A%mat, &
                           matrix_B => this%model_B%mat, &
                           N => this%model_size)
          inner:do i = 1,N
               do j = 1,N
                    matrix_R(i,j) = matrix_A(i,j) &
                           - matrix_B(i,j)

end subroutine sub
