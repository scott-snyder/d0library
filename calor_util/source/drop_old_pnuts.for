      SUBROUTINE DROP_OLD_PNUTS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : HISTOGRAM AND DROP OLD PNUT BANKS 
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   3-OCT-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LPNUT,GZPNUT
C----------------------------------------------------------------------
      LPNUT = GZPNUT(0)
C
      CALL MZDROP(IXCOM,LPNUT,'L') 
C
  999 RETURN
      END
