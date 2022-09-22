      subroutine gtsrcp_i(name, ival, idx)
      implicit none
      character*(*) name
      integer ival(*)
      integer idx
      call gtsrcp (name, ival, idx)
      return
      end
      
