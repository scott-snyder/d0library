      INTEGER FUNCTION GZCGN1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Finds link to Zebra Bank CGN1
C-
C-   Returned value  : Link to bank
C-
C-   Created  29-JUN-1990   Chip Stewart   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------

      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCGN1.LINK'
      INTEGER GZCGNH
C----------------------------------------------------------------------
      GZCGN1 = 0
      LCGNH = GZCGNH()
      IF ( LCGNH.GT.0 ) THEN
        GZCGN1 = LC(LCGNH-IZCGN1)
      ENDIF
  999 RETURN
      END

