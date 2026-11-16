function formula(self)
   x = dot_product(a,   (self%v(:,1) .cross. self%v(:,2)) &
                      + (self%v(:,2) .cross. self%v(:,3)) &
                      + (self%v(:,3) .cross. self%v(:,1)))

   x = dot_product(a, &
                   (self%v(:,1) .cross. self%v(:,2)) &
                      + (self%v(:,2) .cross. self%v(:,3)) &
                      + (self%v(:,3) .cross. self%v(:,1)))

end function face_windingSign_
