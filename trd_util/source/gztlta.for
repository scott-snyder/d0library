      FUNCTION GZTLTA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TLTA bank
C-
C-   Returned value  : Link to 1st element of TLTA linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 08:58:49.45  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTLTA
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTLTA.LINK'
C----------------------------------------------------------------------
      INTEGER GZTLIK
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTLTA = 0
C
C--   GET LINK TO SUPPORTING TLIK BANK
      LTLIK = GZTLIK()
C
C--   CHECK LTLIK
      IF ( LTLIK .LE. 0 ) THEN
        CALL ERRMSG('TLIK-NOT-FOUND','GZTLTA',
     &    'TLIK BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TLTA
      GZTLTA = LC(LTLIK-IZTLTA)
C
  999 RETURN
      END
