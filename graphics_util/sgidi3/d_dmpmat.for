      subroutine d_dmpmat(string,tmat)
      include 'D0$GRAPHICS_UTIL$SGIDI3:DI3GL.INC'
      character*(*) string
      real tmat(4,4)
      if(idebwr.eq.0) return
      write(idebwr,100) string
 100  format(' ',a)
      do i=1,4
        write(idebwr,200) (tmat(i,k),k=1,4)
 200    format('    ',4f)
      enddo
      end
