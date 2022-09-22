      subroutine ffkey_i1 (name, x, n, typ)
      implicit none
      character*(*) name
      integer x
      integer n
      character*(*) typ
      call ffkey (name, x, n, typ)
      return
      end
      
