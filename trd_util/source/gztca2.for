      INTEGER FUNCTION GZTCA2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TCA2 bank
C-
C-   Returned value  : Link to 1st element of TCA2 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCA2.LINK/LIST'
      INTEGER LTCHA,GZTCHA
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTCA2=0
C
C--   GET LINK TO SUPPORTING TCHA BANK
      LTCHA=GZTCHA()
C
C--   CHECK LTCHA
      IF(LTCHA.LE.0)THEN
        CALL ERRMSG('GZTCA2',
     &    'TCHA BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TCA2
      GZTCA2=LC(LTCHA-IZTCA2)
C
  999 RETURN
      END

