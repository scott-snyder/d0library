      FUNCTION GZCSFW()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to CSFW bank
C-
C-   Returned value  : Link CSFW bank
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   3-MAR-1992 09:30:41.38  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZCSFW.LINK/LIST'
      INTEGER GZCSFW,LCSFH,GZCSFH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZCSFW=0
C
C--   GET LINK TO SUPPORTING CSFH BANK
      LCSFH=GZCSFH()
C
C--   CHECK LCSFH
      IF(LCSFH.LE.0)THEN
        CALL ERRMSG('CALORIMETER','GZCSFW',
     &    'CSFH BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO CSFW
      GZCSFW=LC(LCSFH-IZCSFW)
C
  999 RETURN
      END
