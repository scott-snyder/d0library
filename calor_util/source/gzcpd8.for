
      INTEGER FUNCTION GZCPD8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to Zebra Bank CPD8
C-
C-   Returned value  : Link to bank
C-
C-   Created 27-MAR-1990   W.G.D. DHARMARATNA   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCPD8.LINK'
      INTEGER GZCPDH
C----------------------------------------------------------------------
      GZCPD8 = 0
      LCPDH = GZCPDH()
      IF ( LCPDH.GT.0 ) THEN
        GZCPD8 = LC(LCPDH-IZCPD8)
      ENDIF
  999 RETURN
      END



