      FUNCTION GZPVES(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Get pointer to PVES bank I
C-   Inputs  : 
C-      I = bank number
C-
C-   Created  21-JUL-1990   Daria Zieminska
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPVES.LINK'
      INTEGER GZPVES,I
      INTEGER GZPARH,LPARH,LPVES
C----------------------------------------------------------------------
C
      LPARH=GZPARH()
      LPVES=0
C
      IF(LPARH.NE.0)  THEN
        LPVES=LPARH-IZPVES
C          find pointer to requested bank
    1   CONTINUE
        LPVES=LQ(LPVES)
        IF(IQ(LPVES-5).NE.I.AND.LPVES.NE.0) GOTO 1
      ENDIF
C
      GZPVES=LPVES
C
  999 RETURN
      END
