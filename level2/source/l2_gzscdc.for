      FUNCTION L2_GZSCDC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank SCDC in Level 2
C-
C-   Returned value  : pointer to Zebra bank SCDC (under the bank SL2H)
C-
C-   Created  10-AUG-1992   Qizhong Li-Demarteau   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZSL2H.LINK'
      INTEGER L2_GZSCDC, LSL2H
C----------------------------------------------------------------------
      l2_GZSCDC = 0
      IF (LSTPH .GT. 0) THEN
        LSL2H = LC(LSTPH - IZSL2H)
        IF (LSL2H .GT. 0) l2_GZSCDC = LC(LSL2H - 9)
      ENDIF
C
  999 RETURN
      END
