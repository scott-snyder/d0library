      FUNCTION GZPDIL(IDIL)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Get pointer to PDIL bank IDIL
C-   Inputs  : 
C-      IDIL    = bank number
C-
C-   Created 4-DEC-1991   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPDIL.LINK'
      INTEGER GZPDIL,IDIL
      INTEGER GZPARH,LPARH,LPDIL
C----------------------------------------------------------------------
C
      LPARH=GZPARH()
      LPDIL=0
      IF(LPARH.NE.0)  THEN
        LPDIL=LPARH-IZPDIL
    1   CONTINUE
        LPDIL=LQ(LPDIL)
        IF(LPDIL.GT.0.AND.IDIL.GT.0) THEN
          IF(IQ(LPDIL-5).NE.IDIL) GOTO 1
        END IF
      ENDIF
      GZPDIL=LPDIL
  999 RETURN
      END
