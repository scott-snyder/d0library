      INTEGER FUNCTION GZTCA1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TCA1 bank
C-
C-   Returned value  : Link to 1st element of TCA1 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCA1.LINK/LIST'
      INTEGER LTCHA,GZTCHA
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTCA1=0
C
C--   GET LINK TO SUPPORTING TCHA BANK
      LTCHA=GZTCHA()
C
C--   CHECK LTCHA
      IF(LTCHA.LE.0)THEN
        CALL ERRMSG('GZTCA1',
     &    'TCHA BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TCA1
      GZTCA1=LC(LTCHA-IZTCA1)
C
  999 RETURN
      END

