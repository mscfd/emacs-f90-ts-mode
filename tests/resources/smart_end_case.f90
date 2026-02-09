subroutine lower_case()
     call some_thing()
     fff:IF (abc) then
          uvw: if (uvw) then
               If (trs) then
               end
          end
     end
end subroutine lower_case

SUBROUTINE ALL_UPPER()
     call some_thing_else()
end subroutine all_upper

Subroutine Title_case()
     call some_thing_new()
end subroutine title_case


module lower_mod
type :: lower_t
end type lower_t
TYPE :: upper_t
end type upper_t
Type :: title_t
end type title_t
end module lower_mod

MODULE UPPER_MOD
type :: lower_t
end
TYPE :: upper_t
end
Type :: title_t
end
contains
logical function  lower_case()
     call some_thing()
end function lower_case

logical FUNCTION ALL_UPPER()
     call some_thing_else()
end function all_upper

logical Function Title_case()
     call some_thing_new()
end function title_case
end

Module title_mod
 interface lower_i
      module procedure xyz
      module procedure uvw
 end interface lower_i
 INTERFACE UPPER_iface
      module procedure xyz
      module procedure uvw
 end interface upper_iface
 Interface Title_iface
      module procedure xyz
      module procedure uvw
 end interface title_iface

 abstract interface
      real(real32) function f_ifc(x)
           import real32
           real(real32), intent(in) :: x
      end function f_ifc
 end
 ABSTRACT INTERFACE
      real(real32) FUNCTION g_ifc(x)
           import real32
           real(real32), intent(in) :: x
      end
 end
 Abstract Interface
      real(real32) Function h_ifc(x)
           import real32
           real(real32), intent(in) :: x
      end
 end

interface operator(.cross.)
   module procedure cross_product
end ! add the (.cross.)
INTERFACE OPERATOR(.cross.)
   module procedure cross_product
end! add the (.cross.)
Interface Operator(.cross.)
   module procedure cross_product
end     ! add the (.cross.)

interface operator(//)
   module procedure concat_word
end
INTERFACE OPERATOR(<=)
   module procedure compare_word
end
Interface Operator(**)
   module procedure multiply_word
end

interface assignment(=)
   module procedure assign_obj_X
   module procedure assign_obj_Y
end
INTERFACE ASSIGNMENT(=)
   module procedure assign_obj_X
   module procedure assign_obj_Y
end
Interface Assignment(=)
   module procedure assign_obj_X
   module procedure assign_obj_Y
end

end module

submodule (lowerCase) lowerCase_smod
CONTAINS
 SUBROUTINE level1()
 contains
    Subroutine level2()
    contains
       subroutine level3()
       END SUBROUTINE level3
    end subroutine level2
 end subroutine level1
end


SUBMODULE (upperCase) upperCase_smod
CONTAINS
 integer FUNCTION level1()
 contains
    integer Function level2()
    contains
       integer function level3()
       END FUNCTION level3
    end function level2
 end function level1
end submodule


Submodule (TitleCase) TitleCase_smod
Contains
 SUBROUTINE test_do()
     level1:do WHILE (cond1)
          level2: Do i=1,10
               DO
                    level4: DO j = 1,10
                    end
               End
          End level2
     end LEVEL1
 End Subroutine TEST_DO
end

program lowerprog
 first: associate (i => 5)
      SECOND: ASSOCIATE (j => 6)
           ASSOCIATE (j => 6)
                Third: Associate (k => 7, &
                  l => 8, &
                  m => 9)
                end
           end
      end associate
 end associate
end lower

PROGRAM UPPERPROG
end upper

Program TitleProg

 level_1: block
      LEVEL_2: BLOCK
           Level_3: Block
           End Block Level_3
      END BLOCK LEVEL_2
 end block level_1

 outer: select case (x)
 case (1)
      MIDDLE: SELECT case (y)
      case (2)
           Inner: Select CASE (z)
           case default
           End Select Inner
      END SELECT MIDDLE
 end select outer

 BIG: SELECT type (x)
 type is (s)
      Somewhat: Select Type (y)
      class is (t)
           small: select Type (z)
           class is (unknown)
           END
      END
 END


end title
