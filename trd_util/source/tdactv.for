      SUBROUTINE TDACTV
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Deactivate temporary link area for the TRD
C-                                         reconstruction
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   8-DEC-1989   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GTRHLN.INC'
      INCLUDE 'D0$INC:TRDLNK.INC'
      INCLUDE 'D0$INC:LTRD.INC'
      INCLUDE 'D0$INC:TRPROL.INC'
      INCLUDE 'D0$INC:TRTOBN.INC'
C----------------------------------------------------------------------
      GTRHLN(1)=0
      TRDLNK(1)=0
      LTRD(1)  =0
      TRPROL(1)=0
      TRTOBN(1)=0
  999 RETURN
      END
