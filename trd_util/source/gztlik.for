      FUNCTION GZTLIK()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TLIK bank
C-
C-   Returned value  : Link to 1st element of TLIK linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 08:44:57.58  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTLIK
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTLIK.LINK'
C----------------------------------------------------------------------
      INTEGER GZTGEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTLIK = 0
C
C--   GET LINK TO SUPPORTING TGEN BANK
C      LTGEN = GZTGEN()
C
C--   CHECK LTGEN
      IF ( LTGEN .LE. 0 ) THEN
        CALL ERRMSG('TGEN-NOT-FOUND','GZTLIK',
     &    'TGEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TLIK
      GZTLIK = LC(LTGEN-IZTLIK)
C
  999 RETURN
      END
