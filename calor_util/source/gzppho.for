      INTEGER FUNCTION GZPPHO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to PPHO bank
C-
C-   Returned value  : Link to 1st element of PPHO linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-APR-1990 11:48:48.44  Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZPPHO.LINK/LIST'
      INTEGER LPARH,GZPARH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZPPHO=0
C
C--   GET LINK TO SUPPORTING PARH BANK
      LPARH=GZPARH()
C
C--   CHECK LPARH
      IF(LPARH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZPPHO',
     &    'PARH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO PPHO
      GZPPHO=LQ(LPARH-IZPPHO)

C
  999 RETURN
      END
