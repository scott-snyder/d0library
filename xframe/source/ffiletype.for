      SUBROUTINE FFILETYPE(mode,type)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     sets whether you have "selected" fzrz file (type=0),
c     stp file (type=1), or output file (type=2).
C-
C-   Inputs  : mode:  0=fetch, 1=set
C-             type:  0=fz, 1=stp, 2=output
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer mode,type
      character*80 msg
c
c     fetch?
c
      if (mode.eq.0) then
        type = file_type
        return
      endif
c
c     set
c
      if (type.lt.0.or.type.gt.2) then
        write(msg,'('' ILLEGAL TAG ??? '',i9)') type
        CALL xerrmsg(msg)
      else
        file_type = type
      endif
c
      return
      end
