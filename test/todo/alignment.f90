function formula(self)
   x = dot_product(a,   (self%v(:,1) .cross. self%v(:,2)) &
                      + (self%v(:,2) .cross. self%v(:,3)) &
                      + (self%v(:,3) .cross. self%v(:,1)))

   x = dot_product(a, &
                   (self%v(:,1) .cross. self%v(:,2)) &
                      + (self%v(:,2) .cross. self%v(:,3)) &
                      + (self%v(:,3) .cross. self%v(:,1)))

end function face_windingSign_


! alignment of keywords below concurrent
! alignment within reduce, shared, local, local_init
program int23
 integrate: do, concurrent (i = 1:n) default(none) &
        shared(x, y, z, scale) &
        local(tmp) &
        local_init(test) &
        reduce(+: int2, &
        int3)
      tmp  = (real(i)-0.5) * scale
      x(i) = tmp
      y(i) = tmp**2
      z(i) = tmp**3
      int2 = int2 + y(i)
      int3 = int3 + z(i)
 end do integrate
end program int23
