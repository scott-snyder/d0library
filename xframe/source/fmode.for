      SUBROUTINE FMODE(mode)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets mode:  0=exchange, 1=native
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
      fzrz_mode = mode
c
      return
      end
