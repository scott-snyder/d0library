      FUNCTION GZTND1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TND1 bank
C-
C-   Returned value  : Link to 1st element of TND1 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1996 22:18:24.71  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTND1
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTND1.LINK'
C----------------------------------------------------------------------
      INTEGER LTPHY,GZTPHY
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTND1 = 0
C
C--   GET LINK TO SUPPORTING TPHY BANK
      LTPHY = GZTPHY()
C
C--   CHECK LTPHY
      IF ( LTPHY .LE. 0 ) THEN
        CALL ERRMSG('TPHY-NOT-FOUND','GZTND1',
     &    'TPHY BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TND1
      GZTND1 = LC(LTPHY-IZTND1)
C
  999 RETURN
      END
