      SUBROUTINE FINTERRUPT(dummy)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
c     allows "asynchronous" servicing of interrupts via HALT button
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
      integer dummy
c
      halt = .true.
c
      return
      end
