      FUNCTION GZL2FWTB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FWTB 
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
      INCLUDE 'D0$LINKS:IZFWTB.LINK'
      INTEGER GZL2FWTB
      INTEGER GZL2FWAL, LKFWAL
C----------------------------------------------------------------------
      LFWTB = 0
      LKFWAL=GZL2FWAL()
      IF ( LKFWAL .NE. 0 ) LFWTB=LC(LKFWAL-IZFWTB)
      GZL2FWTB=LFWTB
C----------------------------------------------------------------------
  999 RETURN
      END
