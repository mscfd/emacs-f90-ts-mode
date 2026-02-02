subroutine simulate(set, fsim1, &
                         fsim2, out)
     real(kind=real32), dimension(:), &
                        target, &
                        
                        intent(in) :: set
     integer, dimension(:), &
              allocatable, &
              intent(out) :: fsim1, &
                             fsim2
     
     procedure(simulator_ifc) :: &
      fsim

     call sim_init(set, &
                   out)
     call sim_run(fsim, set, &
                        out)
end subroutine simulate
