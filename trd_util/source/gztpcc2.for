      INTEGER FUNCTION GZTPCC2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPCC2 bank
C-
C-   Returned value  : Link to 1st element of TPCC2 
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
      INTEGER LTPEC2,GZTPEC2
C----------------------------------------------------------------------
C
      GZTPCC2=0
      LTPEC2=GZTPEC2()
      IF(LTPEC2.LE.0)THEN
        CALL ERRMSG('GZTPCC2:TPEC2 BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
      GZTPCC2=LC(LTPEC2-IZTPCC)
  999 RETURN
      END

