      INTEGER FUNCTION GZCDD2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank CDD2 
C-
C-   Returned value  : pointer to Zebra bank CDD2 
C-
C-   Created  19-APR-1989   Qizhong Li-Demarteau
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$LINKS:IZCDD2.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
      GZCDD2 = 0
      IF (LHEAD .GT. 0) GZCDD2 = LQ(LHEAD - IZCDD2)
C----------------------------------------------------------------------
  999 RETURN
      END
