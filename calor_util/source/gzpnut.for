      FUNCTION GZPNUT(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Get pointer to PNUT bank I
C-   Inputs  : 
C-      I = bank number, if I=0 return first bank (last booked)
C-
C-   Created  18-JAN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPNUT.LINK'
      INTEGER GZPNUT,I
      INTEGER GZPARH,LPARH,LPNUT
C----------------------------------------------------------------------
C
      LPARH=GZPARH()
      LPNUT=0
C
      IF(LPARH.NE.0)  THEN
        LPNUT=LPARH-IZPNUT
C          find pointer to requested bank
    1   CONTINUE
        LPNUT=LQ(LPNUT)
        IF(IQ(LPNUT-5).NE.I.AND.LPNUT.NE.0.AND.I.NE.0) GOTO 1
      ENDIF
C
      GZPNUT=LPNUT
C
  999 RETURN
      END
