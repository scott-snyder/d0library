      INTEGER FUNCTION GZCPB1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPB1 (calib bad chn, x1)
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
      INCLUDE 'D0$LINKS:IZCPB1.LINK'
      INTEGER GZCPD1,LCPD1
C----------------------------------------------------------------------
      GZCPB1 = 0
      LCPD1 = GZCPD1()
      IF ( LCPD1.GT.0 ) THEN
        GZCPB1 = LC(LCPD1-IZCPB1)
      ELSE
        CALL INTMSG(' -W- GTCPB1 - Error finding CPD1 bank')
      ENDIF
C
  999 RETURN
      END
