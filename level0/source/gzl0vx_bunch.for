      FUNCTION GZL0VX_BUNCH(IBUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank L0VX with 
C-                         bunch number IBUNCH
C-
C-   Returned value  : GZL0VX_BUNCH
C-   Inputs  : IBUNCH
C-   Outputs : None
C-   Controls: None
C-
C-   Created  14-JUL-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      INTEGER GZL0VX_BUNCH
      INTEGER IBUNCH
      INTEGER LKL0VX, LJL0VX
      INTEGER GZL0VX,LZFIND
      EXTERNAL GZL0VX,LZFIND
C
C----------------------------------------------------------------------
      GZL0VX_BUNCH = 0
C
      LKL0VX = GZL0VX()
C
      LJL0VX = LZFIND(IXCOM,LKL0VX,IBUNCH,1)
C
      IF ( LJL0VX.GT.0 ) GZL0VX_BUNCH = LJL0VX
C
C----------------------------------------------------------------------
  999 RETURN
      END
