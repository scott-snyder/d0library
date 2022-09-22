      subroutine ezget_i (char1, ival, idx)
      implicit none
      character*(*) char1
      integer ival(*)
      integer idx
      call ezget (char1, ival, idx)
      return
      end
