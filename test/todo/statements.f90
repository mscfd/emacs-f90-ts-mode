! procedure statements in submodule

submodule (do_mod) do_sub
contains
 module procedure do_fun
      with_exit_: do
           if (i > 10) exit
           i = i + 1
      end do with_exit_
 end procedure
end submodule do_sub
