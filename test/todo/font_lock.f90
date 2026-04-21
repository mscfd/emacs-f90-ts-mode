function eval() result(w)
     call algorithm(fun1(a, b), fun2( &
                                     x, y, &
                                     ! comment

                                     z &
                                       ), &
                    fun3(u, &
                         v, &
                         w))
end function eval
