      INTEGER FUNCTION GZUTAG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to UTAG bank
C-
C-   Returned value  :  link to UTAG
C-   Inputs  :          NONE
C-   Outputs :          NONE
C-   Controls:          NONE
C-
C-   Created  31-JAN-1994   Ulrich Heintz   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUTAG.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
      LANLS=GZANLS()
      IF (LANLS.LE.0) THEN
        GZUTAG=0
      ELSE
        GZUTAG=LQ(LANLS-IZUTAG)
      ENDIF

  999 RETURN
      END
