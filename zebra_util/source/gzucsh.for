      INTEGER FUNCTION GZUCSH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to UCSH bank
C-
C-   Returned value  :  link to UCSH
C-   Inputs  :          NONE
C-   Outputs :          NONE
C-   Controls:          NONE
C-
C-   Created   7-DEC-1993   Ian Adam
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUCSH.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
      LANLS=GZANLS()
      IF (LANLS.LE.0) THEN
        GZUCSH=0
      ELSE
        GZUCSH=LQ(LANLS-IZUCSH)
      ENDIF

  999 RETURN
      END
