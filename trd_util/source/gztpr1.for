      INTEGER FUNCTION GZTPR1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPR1 bank
C-
C-   Returned value  : Link to 1st element of TPR1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPR1.LINK/LIST'
      INTEGER LTPRO,GZTPRO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPR1=0
C
C--   GET LINK TO SUPPORTING TPRO BANK
      LTPRO=GZTPRO()
C
C--   CHECK LTPRO
      IF(LTPRO.LE.0)THEN
        CALL ERRMSG('GZTPR1:TPRO BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPR1
      GZTPR1=LC(LTPRO-IZTPR1)
C
  999 RETURN
      END

