      INTEGER FUNCTION GZTPCA1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPCA1 bank
C-
C-   Returned value  : Link to 1st element of TPCA1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPCA.LINK/LIST'
      INTEGER LTPEC1,GZTPEC1
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPCA1=0
C
C--   GET LINK TO SUPPORTING TPEC1 BANK
      LTPEC1=GZTPEC1()
C
C--   CHECK LTPEC1
      IF(LTPEC1.LE.0)THEN
        CALL ERRMSG('GZTPCA1:TPEC1 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPCA1
      GZTPCA1=LC(LTPEC1-IZTPCA)
C
  999 RETURN
      END

