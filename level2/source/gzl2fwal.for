      FUNCTION GZL2FWAL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FWAL 
C-
C-   Returned value  : 
C-   Inputs  : none
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUN-1992   Yi-Cheng Liu ( for Level2 stuff )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INCLUDE 'D0$LINKS:IZFWAL.LINK'
      INTEGER GZL2FWAL
      INTEGER GZL2FGEH, LKFGEH
C----------------------------------------------------------------------
      LFWAL = 0
      LKFGEH=GZL2FGEH()
      IF ( LKFGEH .NE. 0 ) LFWAL=LC(LKFGEH-IZFWAL)
      GZL2FWAL=LFWAL
C----------------------------------------------------------------------
  999 RETURN
      END
