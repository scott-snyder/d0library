      INTEGER FUNCTION GZCPZ8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPZ8 (calib zsp bank, x8)
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
      INCLUDE 'D0$LINKS:IZCPZ8.LINK'
      INTEGER GZCPD8,LCPD8
C----------------------------------------------------------------------
      GZCPZ8 = 0
      LCPD8 = GZCPD8()
      IF ( LCPD8.GT.0 ) THEN
        GZCPZ8 = LC(LCPD8-IZCPZ8)
      ELSE
        CALL INTMSG(' -W- GTCPZ8 - Error finding CPD8 bank')
      ENDIF
C
  999 RETURN
      END
