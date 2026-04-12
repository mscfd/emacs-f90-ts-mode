program coarray_test
 implicit none

 integer :: scalar_co[*]
 real    :: arr(10)[2, *]
 real, codimension[2,*], allocatable :: arr2(10)
 real, dimension(1:10), codimension[1:2,*], pointer :: arr3
 integer, codimension[1:3,1:4,*] :: val
 integer :: team_value[*]
 type(lock_type), codimension[*] :: mylock

 scalar_co[1]       = 42
 arr(5)[1, 2]       = 3.14
 val[this_image()]  = this_image()

 ! intrinsics
 print *, num_images()
 print *, this_image()
 print *, image_index(arr, [1, 2])

 ! synchronization
 sync all
 sync images(*)
 sync images([1, 2, 3])
 sync memory

 ! critical section
 critical
 scalar_co[1] = scalar_co[1] + 1
 end critical

 ! team
 form team (mod(this_image(), 2) + 1, team_value)
 change team (team_value)
 print *, "In team, image:", this_image()
 end team

end program coarray_test


program sum_square
 use, intrinsic :: iso_fortran_env, only: lock_type
  use iso_fortran_env, only : STAT_FAILED_IMAGE,  STAT_STOPPED_IMAGE

 implicit none
 type(lock_type), codimension[*] :: slock
 integer, codimension[*] :: ssq
 integer :: sync_stat, alloc_stat

 if (this_image() == 1) then
      i = 1
 end if
 sync all (stat=sync_stat)
 if (stat /= 0) then
      if (stat == STAT_FAILED_IMAGE) then
           print *,"Failed images: ", failed_images()
      else if (stat == STAT_STOPPED_IMAGE) then
      print *,"Stopped images: ", stopped_images()
      else
           print *,"Unforseen error, aborting"
           error stop
      end if
 end if

 block
      logical :: gotit
      do
           lock(slock[1], acquired_lock=gotit)
           if (gotit) exit
      end do
 end block
 ssq[1] = this_image()**2 + ssq[1]
 unlock (slock[1])
 call co_broadcast(ssq, 1)
 print *, this_image(), ssq

 ssq = this_image()**2
 call co_sum(ssq, result_image=1)
 call co_min(ssq)
 call co_max(ssq)
 call co_reduce(ssq, redfun)

 print *, this_image(), co_min(ssq), co_max(ssq)
 if (this_image() == 1) then
      print *, this_image(), co_sum(ssq, result_image=1)
 end if
end program sum_square

program event_with_wait
 use iso_fortran_env, only: event_type
 implicit none

 type(event_type) :: evt[*]
 integer :: me

 me = this_image()

 if (num_images() < 2) then
      if (me == 1) print *, "Run with at least 2 images"
      stop
 end if

 if (me == 1) then
      print *, "image 1 waiting for TWO events..."

      ! Wait until at least 2 events have been posted
      call event_wait(evt, until_count=2)

      print *, "image 1 received 2 events!"
    
 else if (me == 2) then
 print *, "image 2 posting events..."

 call event_post(evt[1])
 print *, "Posted first event"

 call event_post(evt[1])
 print *, "Posted second event"
 end if

end program event_with_wait


program fail_atom_event_example
 use iso_fortran_env, only: event_type, atomic_int_kind
 implicit none

 type(event_type) :: evt[*]
 integer(atomic_int_kind) :: flag[*]   ! shared atomic flag
 integer :: me, val
 logical :: done

 me = this_image()

 if (num_images() < 2) then
      if (me == 1) print *, "Run with at least 2 images"
      stop
 end if

 if (me == 2) then
      ! producer (image 2)
      print *, "image 2: starting work"

      call atomic_define(flag[1], 42)
      print *, "image 2: wrote value"

      call event_post(evt[1])
      print *, "image 2: notified image 1"

      ! simulate failure after notifying
      print *, "image 2: failing now"
      fail image

 else if (me == 1) then
 ! consumer (image 1)
 print *, "image 1: waiting for notification..."

 done = .false.

 do while (.not. done)
      ! wait for event (may return even if image 2 later fails)
      call event_wait(evt)

      ! check if image 2 is still alive
      if (this_image() /= 2 .and. failed_images() /= 0) then
           print *, "image 1: detected a failed image"
      end if

      ! safely read atomic value
      call atomic_ref(val, flag)

      print *, "image 1: received value =", val
      done = .true.

 end do
 end if

end program fail_atom_event_example
