      FUNCTION GZL0SC_BUNCH(IBUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank L0SC with 
C-                         bunch number IBUNCH
C-
C-   Returned value  : GZL0SC_BUNCH
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
      INTEGER GZL0SC_BUNCH
      INTEGER IBUNCH
      INTEGER LKL0SC, LJL0SC
      INTEGER GZL0SC,LZFIND
      EXTERNAL GZL0SC,LZFIND
C
C----------------------------------------------------------------------
      GZL0SC_BUNCH = 0
C
      LKL0SC = GZL0SC()
C
      LJL0SC = LZFIND(IXCOM,LKL0SC,IBUNCH,1)
C
      IF ( LJL0SC.GT.0 ) GZL0SC_BUNCH = LJL0SC
C
C----------------------------------------------------------------------
  999 RETURN
      END
