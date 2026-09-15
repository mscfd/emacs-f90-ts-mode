 subroutine error6()
      call foo(arg1, &
!                  ^ (f90-ts-font-lock-delimiter-face f90-ts-font-lock-error-face)
!                    ^ f90-ts-font-lock-delimiter-face
 !!$  &           arg2, &
!^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
         &   )
!        ^ f90-ts-font-lock-delimiter-face
!            ^ f90-ts-font-lock-bracket-face
 end subroutine error6
