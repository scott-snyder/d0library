      INTEGER FUNCTION GZTPEC1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPEC1 bank
C-
C-   Returned value  : Link to 1st element of TPEC1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPEC.LINK/LIST'
      INTEGER LTPR1,GZTPR1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPEC1=0
C
C--   GET LINK TO SUPPORTING TPR1 BANK
      LTPR1=GZTPR1()
C
C--   CHECK LTPR1
      IF(LTPR1.LE.0)THEN
        CALL ERRMSG('GZTPEC1:TPR1 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPEC1
      GZTPEC1=LC(LTPR1-IZTPEC)
C
  999 RETURN
      END

