subroutine foo()
     name: if (condition) then
          call foo()
     elseif (x > y) then
          ! comment
     elseif (a .eqv. b) then

          if (condition .or. .false.) x = fun1(y)
          if (2*5 > x) then
               print *, x
          end if
          inner: if (.true.) then
               ! comment
          end if inner
     else

     end if name
end subroutine foo
