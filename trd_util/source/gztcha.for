      INTEGER FUNCTION GZTCHA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TCHA bank
C-
C-   Returned value  : Link to 1st element of TCHA 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCHA.LINK/LIST'
      INTEGER LTPHY,GZTPHY
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTCHA=0
C
C--   GET LINK TO SUPPORTING TPHY BANK
      LTPHY=GZTPHY()
C
C--   CHECK LTPHY
      IF(LTPHY.LE.0)THEN
        CALL ERRMSG('GZTCHA',
     &    'TPHY BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TCHA
      GZTCHA=LC(LTPHY-IZTCHA)
C
  999 RETURN
      END

