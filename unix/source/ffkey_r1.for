      subroutine ffkey_r1 (name, x, n, typ)
      implicit none
      character*(*) name
      real x
      integer n
      character*(*) typ
      call ffkey (name, x, n, typ)
      return
      end
      
