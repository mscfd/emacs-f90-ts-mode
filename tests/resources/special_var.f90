function fun(self, other_self) result(this_result)
     logical :: this_result
     class(tMyself), intent(in) :: self
     class(tThisType), intent(out) :: other_self

     call copy(self, other_self)
     this_result = other_self%check()
end function fun

subroutine sub(this, x)
     class(yourself_t), intent(inout) :: this
     integer, intent(in) :: x
     call self%set_val(x)
end subroutine sub
