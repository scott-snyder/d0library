      SUBROUTINE FOENABLE(dum)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets enable for offset
C-
C-   Inputs  : dum - 0=accoff, 1=acclin
C-   Outputs :
C-   Controls:
C-
C-   Created  24-AUG-1992   Drew Baden
C-
      implicit none
c
      integer dum
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      if (dum.eq.0) then
        accoff = .true.
      else
        acclin = .true.
      endif
c
      return
      end
