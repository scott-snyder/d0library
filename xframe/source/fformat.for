      SUBROUTINE FFORMAT(mode)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets the format (0=auto, 1=int, 2=float, 3=hex)
C-
C-   Inputs  :
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
      iformt = mode
c
      return
      end
