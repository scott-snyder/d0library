      SUBROUTINE FSETOFF(mode)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets the offset for navigating
C-
C-   Inputs  : mode      - offset (in lq(lbank+offset)
C-   Outputs :
C-   Controls:
C-
C-   Created  23-AUG-1992   Drew Baden
C-
      implicit none
c
      include 'D0$XFRAME$SOURCE:d0map.inc'
c
      integer mode
c
      ifoff = mode
c
      return
      end
