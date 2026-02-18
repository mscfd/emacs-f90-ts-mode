 subroutine sub_omp()
!^^^^^^^^^^ font-lock-keyword-face
!           ^^^^^^^ font-lock-function-name-face
!                  ^^ f90-ts-font-lock-bracket-face
      integer :: i
!     ^^^^^^^ font-lock-type-face
!             ^^ f90-ts-font-lock-delimiter-face
!                ^ nil
      ! indentation within openmp block not implemented
!     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ font-lock-comment-face
 !$   logical :: flag
!^^^^^^^^^^^^^^^^^^^^ f90-ts-font-lock-openmp-face

 !$omp parallel
!^^^^^^^^^^^^^^ f90-ts-font-lock-openmp-face
      flag = .false.
!     ^^^^ nil
!          ^ f90-ts-font-lock-operator-face
!            ^^^^^^^ font-lock-constant-face
 !$   flag = .true.
!^^^^^^^^^^^^^^^^^^ f90-ts-font-lock-openmp-face
      if (flag) then
!     ^^ font-lock-keyword-face
!        ^ f90-ts-font-lock-bracket-face
!         ^^^^ nil
!             ^ f90-ts-font-lock-bracket-face
!               ^^^^ font-lock-keyword-face
           print *,"openmp"
!          ^^^^^ font-lock-builtin-face
!                ^ nil
!                 ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^^^ font-lock-string-face
      else
!     ^^^^ font-lock-keyword-face
           print *,"no openmp"
!          ^^^^^ font-lock-builtin-face
!                ^ nil
!                 ^ f90-ts-font-lock-delimiter-face
!                  ^^^^^^^^^^^ font-lock-string-face
      end if
!     ^^^ font-lock-keyword-face
!         ^^ font-lock-keyword-face
 !$omp end parallel
!^^^^^^^^^^^^^^^^^^ f90-ts-font-lock-openmp-face
 end subroutine sub_omp
!^^^ font-lock-keyword-face
!    ^^^^^^^^^^ font-lock-keyword-face
!               ^^^^^^^ font-lock-function-name-face
