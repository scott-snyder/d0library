      INTEGER FUNCTION GZTPRO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPRO bank
C-
C-   Returned value  : Link to 1st element of TPRO 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPRO.LINK/LIST'
      INTEGER LTPHY,GZTPHY
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPRO=0
C
C--   GET LINK TO SUPPORTING TPHY BANK
      LTPHY=GZTPHY()
C
C--   CHECK LTPHY
      IF(LTPHY.LE.0)THEN
        CALL ERRMSG('GZTPRO',
     &    'TPHY BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPRO
      GZTPRO=LC(LTPHY-IZTPRO)
C
  999 RETURN
      END

