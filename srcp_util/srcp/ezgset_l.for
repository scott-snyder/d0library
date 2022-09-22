      subroutine ezget_l (char1, ival, idx)
      implicit none
      character*(*) char1
      logical ival(*)
      integer idx
      call ezget (char1, ival, idx)
      return
      end
