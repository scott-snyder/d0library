      INTEGER FUNCTION GZUDST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return link to UDST bank
C-
C-   Returned value  :  link to UDST
C-   Inputs  :          NONE
C-   Outputs :          NONE
C-   Controls:          NONE
C-
C-   Created  27-JAN-1994   Ulrich Heintz   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUDST.LINK'
      INTEGER LANLS,GZANLS
C----------------------------------------------------------------------
      LANLS=GZANLS()
      IF (LANLS.LE.0) THEN
        GZUDST=0
      ELSE
        GZUDST=LQ(LANLS-IZUDST)
      ENDIF
  999 RETURN
      END
