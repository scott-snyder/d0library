      INTEGER FUNCTION GZPELC()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PELC bank
C-
C-   Returned value  : Link to 1st element of PELC linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-APR-1990 11:45:28.16  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPELC.LINK/LIST'
      INTEGER LPARH,GZPARH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPELC=0
C
C--   GET LINK TO SUPPORTING PARH BANK
      LPARH=GZPARH()
C
C--   CHECK LPARH
      IF(LPARH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPELC',
     &    'PARH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO PELC
      GZPELC=LQ(LPARH-IZPELC)

C
  999 RETURN
      END
