      INTEGER FUNCTION GZTPET2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPET2 bank
C-
C-   Returned value  : Link to 1st element of TPET2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPET.LINK/LIST'
      INTEGER LTPR2,GZTPR2
C----------------------------------------------------------------------
C
      GZTPET2=0
      LTPR2=GZTPR2()
      IF(LTPR2.LE.0)THEN
        CALL ERRMSG('GZTPET2:TPR2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
      GZTPET2=LC(LTPR2-IZTPET)
  999 RETURN
      END
