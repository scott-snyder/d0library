      FUNCTION GZTEDE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TEDE bank
C-
C-   Returned value  : Link to 1st element of TEDE linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:14:06.36  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTEDE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTEDE.LINK'
C----------------------------------------------------------------------
      INTEGER LTELE,GZTELE
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTEDE = 0
C
C--   GET LINK TO SUPPORTING TELE BANK
      LTELE = GZTELE()
C
C--   CHECK LTELE
      IF ( LTELE .LE. 0 ) THEN
        CALL ERRMSG('TELE-NOT-FOUND','GZTEDE',
     &    'TELE BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TEDE
      GZTEDE = LC(LTELE-IZTEDE)
C
  999 RETURN
      END
