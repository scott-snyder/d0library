      INTEGER FUNCTION GZTPR2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPR2 bank
C-
C-   Returned value  : Link to 1st element of TPR2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPR2.LINK/LIST'
      INTEGER LTPRO,GZTPRO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPR2=0
C
C--   GET LINK TO SUPPORTING TPRO BANK
      LTPRO=GZTPRO()
C
C--   CHECK LTPRO
      IF(LTPRO.LE.0)THEN
        CALL ERRMSG('GZTPR2:TPRO BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPR2
      GZTPR2=LC(LTPRO-IZTPR2)
C
  999 RETURN
      END

