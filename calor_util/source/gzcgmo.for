      INTEGER FUNCTION GZCGMO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to zebra bank CGMO (calib ped moments)
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
      INCLUDE 'D0$LINKS:IZCGMO.LINK'
      INTEGER GZCGN8,LCGN8
C----------------------------------------------------------------------
      GZCGMO = 0
      LCGN8 = GZCGN8()
      IF ( LCGN8.GT.0 ) THEN
        IF (IC(LCGN8-IZCGMO).GE.IZCGMO) GZCGMO = LC(LCGN8-IZCGMO)
      ENDIF
C
  999 RETURN
      END
