      SUBROUTINE FSETOUT(w,tag,reason)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : called from allout (c), tag = 0 cancel, 
C-                         1=exchange, 2=native
C-                         3=exchange/list, 4=native/list
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  15-DEC-1992   Drew Baden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
c
      include 'd0$xframe$source:d0map.inc'
c
      integer w,tag,reason
      character*80 filename,listname
      character*2 chopt
      logical ok
      integer ilen,nd,nuhead,iuhead(50)
      logical listfile/.false./
c
      write(*,'('' OBSOLETE ROUTINE - SEND EMAIL TO DREW!!!'')')
c
c     that's all folks
c
      return
      end
