      INTEGER FUNCTION GZBERD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to BERD bank
C-
C-   Returned value  : Link to 1st element of BERD linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   2-JUL-1990 17:13:19.16  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZBERD.LINK/LIST'
      INTEGER GZHEAD
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZBERD=0
C
C--   CHECK LHEAD
C
      IF(LHEAD.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZBERD',
     &    'HEAD BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO BERD
      GZBERD=LQ(LHEAD-IZBERD)
C
  999 RETURN
      END
