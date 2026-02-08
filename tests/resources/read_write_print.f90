subroutine print()

     read: associate(x => arr(2))
          read(*,*) x
     end associate read

     print: do value = 1,10
          print *, 'hello', value
     end do print

     write: if (result > 10) then
          write(unit,'(es15.5,f12.3)') expr, result
     end if write

end subroutine print
