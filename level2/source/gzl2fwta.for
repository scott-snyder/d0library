      FUNCTION GZL2FWTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FWTA 
C-
C-   Returned value  : 
C-   Inputs  : none
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFWTA.LINK'
      INTEGER GZL2FWTA
      INTEGER GZL2FWAL, LKFWAL
C----------------------------------------------------------------------
      LFWTA = 0
      LKFWAL=GZL2FWAL()
      IF ( LKFWAL .NE. 0 ) LFWTA=LC(LKFWAL-IZFWTA)
      GZL2FWTA=LFWTA
C----------------------------------------------------------------------
  999 RETURN
      END
