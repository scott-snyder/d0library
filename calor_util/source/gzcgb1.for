      INTEGER FUNCTION GZCGB1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGB1 (calib bad chn, x1)
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
      INCLUDE 'D0$LINKS:IZCGB1.LINK'
      INTEGER GZCGN1,LCGN1
C----------------------------------------------------------------------
      GZCGB1 = 0
      LCGN1 = GZCGN1()
      IF ( LCGN1.GT.0 ) THEN
        GZCGB1 = LC(LCGN1-IZCGB1)
      ELSE
        CALL INTMSG(' -W- GTCGB1 - Error finding CGN1 bank')
      ENDIF
C
  999 RETURN
      END
