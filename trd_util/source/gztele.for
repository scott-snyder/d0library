      FUNCTION GZTELE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TELE bank
C-
C-   Returned value  : Link to 1st element of TELE linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 08:56:14.90  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTELE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTELE.LINK'
C----------------------------------------------------------------------
      INTEGER GZTLIK
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTELE = 0
C
C--   GET LINK TO SUPPORTING TLIK BANK
      LTLIK = GZTLIK()
C
C--   CHECK LTLIK
      IF ( LTLIK .LE. 0 ) THEN
        CALL ERRMSG('TLIK-NOT-FOUND','GZTELE',
     &    'TLIK BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TELE
      GZTELE = LC(LTLIK-IZTELE)
C
  999 RETURN
      END
