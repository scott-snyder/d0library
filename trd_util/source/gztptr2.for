      INTEGER FUNCTION GZTPTR2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPTR2 bank
C-
C-   Returned value  : Link to 1st element of TPTR2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPTR.LINK/LIST'
      INTEGER LTPR2,GZTPR2
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPTR2=0
C
C--   GET LINK TO SUPPORTING TPR2 BANK
      LTPR2=GZTPR2()
C
C--   CHECK LTPR2
      IF(LTPR2.LE.0)THEN
        CALL ERRMSG('GZTPTR2:TPR2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPTR2
      GZTPTR2=LC(LTPR2-IZTPTR)
C
  999 RETURN
      END

