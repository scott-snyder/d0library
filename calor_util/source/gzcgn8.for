      INTEGER FUNCTION GZCGN8()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to Zebra Bank CGN8
C-
C-   Returned value  : Link to bank
C-
C-   Created  29-JUN-1990   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGN8.LINK'
      INTEGER GZCGNH
C----------------------------------------------------------------------
      GZCGN8 = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        GZCGN8 = LC(LCGNH-IZCGN8)
      ENDIF
  999 RETURN
      END

