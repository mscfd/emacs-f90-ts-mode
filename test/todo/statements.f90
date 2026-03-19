! statement: forall
subroutine sub()
     forall (i = 1:n, j = 1:n, a(i,j) /= 0.0)
     b(i,j) = 1.0 / a(i,j)
     c(i,j) = b(i,j) * 2.0
     end forall

     forall (i = 1:n)  a(i,i) = 1.0
end subroutine sub
