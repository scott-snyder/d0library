      FUNCTION GZL2FGEH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank FGEH 
C-
C-   Returned value  : FGEH bank pointer
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  17-MAR-1989   Jeffrey Bantly
C-   Updated  15-JUL-1991   Susan K. Blessing  Remove path checking.
C-    We only use STPC.
C-   Updated  15-JUN-1992   YI-CHENG LIU ( FOR LEVEL2 STUFF )
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$INC:FDCPRM.INC'
      INTEGER GZL2FGEH
      INTEGER GZSL2H,LSL2H
C----------------------------------------------------------------------
      LFGEH = 0              
      LSL2H = GZSL2H()
      IF ( LSL2H .NE. 0 ) LFGEH = LC( LC(LSL2H-13) -4)
C                                         L2FDC    FGEH
      GZL2FGEH=LFGEH
C----------------------------------------------------------------------
  999 RETURN
      END
