      INTEGER FUNCTION GZCPB8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPB8 (calib bad chn, x8)
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
      INCLUDE 'D0$LINKS:IZCPB8.LINK'
      INTEGER GZCPD8,LCPD8
C----------------------------------------------------------------------
      GZCPB8 = 0
      LCPD8 = GZCPD8()
      IF ( LCPD8.GT.0 ) THEN
        GZCPB8 = LC(LCPD8-IZCPB8)
      ELSE
        CALL INTMSG(' -W- GTCPB8 - Error finding CPD8 bank')
      ENDIF
C
  999 RETURN
      END
