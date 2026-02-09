subroutine labels

     if_label: if (i > 10) then
          nested_if: if (cond1 &
                         .and. cond2) &
                 then
               call labels()
          end if nested_if
     end if if_label

     do_label: do i = 1,10
          nested_do: do while (i < 5)
               ! comment

          end do nested_do
     end do do_label

     assoc_label: associate (value1 => 1, &
                             value2 => 2)
          nested_assoc: associate(bar => b+a+r)
               write(u,fmt) bar
          end associate nested_assoc
     end associate assoc_label

     block_label: block
          use my_mod
          integer :: i
          real :: r
          nested_block: block
               use other_mod
               logical :: check
               i = ceiling(r)
          end block nested_block
     end block block_label

     select_case_label: select case (x)
     case (1)
          ! comment 1
     case (2)
          nested_select_case: select case (str)
          case ('action')
               call action()
          case default
               call other()
          end select nested_select_case
     case default
          call default()
     end select select_case_label

     select_type_label: select type (self%obj)
     type is (implementation_A_t)
          call self%obj%compute()
     class is (abstract_B_t)
          call self%obj%evaluate()
     class default
          nested_select_type: select type (self%foo)
          class is (vector_t)
               print *,'have foo'
          end select nested_select_type
     end select select_type_label

end subroutine labels
