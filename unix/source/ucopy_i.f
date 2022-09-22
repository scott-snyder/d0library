      subroutine ucopy_i (a, b, n)
      implicit none
      integer n
      integer a(n), b(n)
      call ucopy(a, b, n)
      return
      end
      
