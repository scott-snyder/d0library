      INTEGER FUNCTION GZCPD1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to Zebra Bank CPD1
C-
C-   Returned value  : Link to bank
C-
C-   Created 27-MAR-1990   W.G.D. DHARMARATNA   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD1.LINK'
      INTEGER GZCPDH
C----------------------------------------------------------------------
      GZCPD1 = 0
      LCPDH = GZCPDH()
      IF ( LCPDH.GT.0 ) THEN
        GZCPD1 = LC(LCPDH-IZCPD1)
      ENDIF
  999 RETURN
      END

