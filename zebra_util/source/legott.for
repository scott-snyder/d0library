      SUBROUTINE LEGOTT(NUMB,TITLE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Return title for LEGO bank number NUMB
C-
C-   Inputs  : 
C-     NUMB= LEGO bank number
C-   Outputs : 
C-    TITLE= title (up to 32 characters long)
C-
C-   Created  15-FEB-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUMB
      CHARACTER*(*) TITLE
      CHARACTER*32 TITL1
      INTEGER LEGO,GZLEGO
C----------------------------------------------------------------------
C
      TITL1=' Does not exist'
      LEGO=GZLEGO(NUMB)
      IF(LEGO.NE.0) CALL UHTOC(IQ(LEGO+3),32,TITL1,32)
C
      TITLE=TITL1
  999 RETURN
      END
