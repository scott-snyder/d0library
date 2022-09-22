      subroutine ffkey_l (name, x, n, typ)
      implicit none
      character*(*) name
      logical x(*)
      integer n
      character*(*) typ
      call ffkey (name, x, n, typ)
      return
      end
      
