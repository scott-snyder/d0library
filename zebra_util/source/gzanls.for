      INTEGER FUNCTION GZANLS()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the link to the ANLS bank 
C-
C-   Returned values  : Link to ANLS bank
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  25-APR-1991   Geoff Forden
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZANLS.LINK'
C----------------------------------------------------------------------
      GZANLS=0
      IF(LHEAD.EQ.0)THEN
        CALL ERRMSG('CALORIMETER','GZANLS',
     &    'HEAD BANK DOES NOT EXIST','W')
        RETURN
      ENDIF
      GZANLS=LQ(LHEAD-IZANLS)
  999 RETURN
      END
