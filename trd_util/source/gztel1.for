      FUNCTION GZTEL1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TEL1 bank
C-
C-   Returned value  : Link to 1st element of TEL1 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:10:13.77  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTEL1
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTEL1.LINK'
C----------------------------------------------------------------------
      INTEGER LTELE,GZTELE
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTEL1 = 0
C
C--   GET LINK TO SUPPORTING TELE BANK
      LTELE = GZTELE()
C
C--   CHECK LTELE
      IF ( LTELE .LE. 0 ) THEN
        CALL ERRMSG('TELE-NOT-FOUND','GZTEL1',
     &    'TELE BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TEL1
      GZTEL1 = LC(LTELE-IZTEL1)
C
  999 RETURN
      END
