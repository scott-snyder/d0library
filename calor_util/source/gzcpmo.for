      INTEGER FUNCTION GZCPMO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CPMO (calib ped moments)
C-
C-   Inputs  : none
C-   Outputs : Link to bank
C-   Controls: none
C-
C-   Created  18-MAY-1992   Jan Guida
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPMO.LINK'
      INTEGER GZCPD8,LCPD8
C----------------------------------------------------------------------
      GZCPMO = 0
      LCPD8 = GZCPD8()
      IF ( LCPD8.GT.0 ) THEN
        IF (IC(LCPD8-IZCPMO).GE.IZCPMO) GZCPMO = LC(LCPD8-IZCPMO)
      ENDIF
C
  999 RETURN
      END
