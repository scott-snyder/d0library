      FUNCTION GZTEL2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TEL2 bank
C-
C-   Returned value  : Link to 1st element of TEL2 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:11:46.87  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTEL2
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTEL2.LINK'
C----------------------------------------------------------------------
      INTEGER LTELE,GZTELE
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTEL2 = 0
C
C--   GET LINK TO SUPPORTING TELE BANK
      LTELE = GZTELE()
C
C--   CHECK LTELE
      IF ( LTELE .LE. 0 ) THEN
        CALL ERRMSG('TELE-NOT-FOUND','GZTEL2',
     &    'TELE BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TEL2
      GZTEL2 = LC(LTELE-IZTEL2)
C
  999 RETURN
      END
