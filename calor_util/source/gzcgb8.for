      INTEGER FUNCTION GZCGB8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGB8 (calib bad chn, x8)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created  12-MAY-1992   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGB8.LINK'
      INTEGER GZCGN8,LCGN8
C----------------------------------------------------------------------
      GZCGB8 = 0
      LCGN8 = GZCGN8()
      IF ( LCGN8.GT.0 ) THEN
        GZCGB8 = LC(LCGN8-IZCGB8)
      ELSE
        CALL INTMSG(' -W- GTCGB8 - Error finding CGN8 bank')
      ENDIF
C
  999 RETURN
      END
