      FUNCTION GZTPIO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPIO bank
C-
C-   Returned value  : Link to 1st element of TPIO linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 08:52:58.90  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTPIO
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPIO.LINK'
C----------------------------------------------------------------------
      INTEGER GZTLIK
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPIO = 0
C
C--   GET LINK TO SUPPORTING TLIK BANK
      LTLIK = GZTLIK()
C
C--   CHECK LTLIK
      IF ( LTLIK .LE. 0 ) THEN
        CALL ERRMSG('TLIK-NOT-FOUND','GZTPIO',
     &    'TLIK BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TPIO
      GZTPIO = LC(LTLIK-IZTPIO)
C
  999 RETURN
      END
