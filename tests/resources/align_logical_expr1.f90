subroutine sub1()
     do while (A1 .and. A2 .and. A3 .and. A4 &
                  .and. B1)
     end do
     do while (A1 .and. A2 .and. A3 .and. A4 &
                           .and. B1)
     end do
     do while (A1 .and. A2 .and. A3 .and. A4 &
                                    .and. B1)
     end do
end subroutine sub1

subroutine sub2()
     do while (A1 .and. (A2 .and. A3) .and. A4 &
                  .and. B1)
     end do
     do while (A1 .and. (A2 .and. A3) .and. A4 &
                                      .and. B1)
     end do
end subroutine sub2

subroutine sub3()
     do while (A1 .and. (A2 .and. A3) .and. A4 &
                  .and. (B1 == B2) &
                  .or. C1)
     end do
     do while (A1 .and. (A2 .and. A3) .and. A4 &
                  .and. (B1 == B2) &
                                      .or. C1)
     end do
end subroutine sub3

subroutine sub4()
     do while (A1 .and. (A2 .and. A3) .and. &
               A4 .and. (B1 == B2) &
                  .or. C1)
     end do
     do while (A1 .and. (A2 .and. A3) .and. &
                        A4 .and. (B1 == B2) &
                           .or. C1)
     end do
     do while (A1 .and. (A2 .and. A3) .and. &
                        A4 .and. (B1 == B2) &
                                      .or. C1)
     end do
end subroutine sub4

subroutine sub5()
     do while (A1 .and. A2 .and. A3 .and. (B1 .or. &
                                           B2))
     end do
end subroutine sub5

subroutine sub6()
     if (A1 .and. A2 .and. (A3 .or. A4) .and. &
         B3 .and. B4 .and. &
         (C1 .or. C2)) then
     end if

     if (A1 .and. A2 .and. (A3 .or. A4) .and. &
                  B3 .and. B4 .and. &
                           (C1 .or. C2)) then
     end if

     if (A1 .and. A2 .and. (A3 .or. A4) .and. &
                           B3 .and. B4 .and. &
                  (C1 .or. C2)) then
     end if
end subroutine sub6

subroutine sub7()
     if (A1 .and. A2 .and. (A3 .or. A4) .and. &
          B1 &
                       .and. C1 .and. C2 &
                     .eqv. D1 .and. D2 &
                               .or. D1 .and. D2 .and. &
                         E1) then
     end if
end subroutine sub7

subroutine sub8()
   flag =     condition1(p1, &
                         p2) &
              .or. condition2(q1, q2, &
                                  q3) &
              .and. (condition3(a1,a2,a3&
                               )&
                     .or. condition4(&
                                    fun1(x,y), &
                                     fun2(u,v)) &
.eqv. condition5(&
                 fun1(x,y), &
                fun2(u,v)) &
                              )&
.neqv. condition6(&
                  fun1(x,y), &
                  fun2(u,v) &
) &
)
end subroutine sub7
