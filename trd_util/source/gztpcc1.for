      INTEGER FUNCTION GZTPCC1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPCC1 bank
C-
C-   Returned value  : Link to 1st element of TPCC1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPCC.LINK/LIST'
      INTEGER LTPEC1,GZTPEC1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPCC1=0
C
C--   GET LINK TO SUPPORTING TPEC1 BANK
      LTPEC1=GZTPEC1()
C
C--   CHECK LTPEC1
      IF(LTPEC1.LE.0)THEN
        CALL ERRMSG('GZTPCC1:TPEC1 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPCC1
      GZTPCC1=LC(LTPEC1-IZTPCC)
C
  999 RETURN
      END

