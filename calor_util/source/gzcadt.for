      FUNCTION GZCADT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CADT bank
C-
C-   Returned value  : Link to 1st element of CADT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-SEP-1990 10:20:16.41  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCADT.LINK/LIST'
      INTEGER GZCADT,GZCGEH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCADT=0
C
C--   GET LINK TO SUPPORTING CGEH BANK
      LCGEH=GZCGEH()
C
C--   CHECK LCGEH
      IF(LCGEH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCADT',
     &    'CGEH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CADT
      GZCADT=LC(LCGEH-IZCADT)
C
  999 RETURN
      END
