      FUNCTION GZL0AD_BUNCH(IBUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank L0AD with 
C-                         bunch number IBUNCH
C-
C-   Returned value  : GZL0AD_BUNCH
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
      INTEGER GZL0AD_BUNCH
      INTEGER IBUNCH
      INTEGER LKL0AD, LJL0AD
      INTEGER GZL0AD,LZFIND
      EXTERNAL GZL0AD,LZFIND
C
C----------------------------------------------------------------------
      GZL0AD_BUNCH = 0
C
      LKL0AD = GZL0AD()
C
      LJL0AD = LZFIND(IXCOM,LKL0AD,IBUNCH,1)
C
      IF ( LJL0AD.GT.0 ) GZL0AD_BUNCH = LJL0AD
C
C----------------------------------------------------------------------
  999 RETURN
      END
