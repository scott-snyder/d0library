      FUNCTION GZTND2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TND2 bank
C-
C-   Returned value  : Link to 1st element of TND2 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  16-APR-1996 22:49:45.89  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTND2
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTND2.LINK'
C----------------------------------------------------------------------
      INTEGER LTPHY,GZTPHY
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTND2 = 0
C
C--   GET LINK TO SUPPORTING TPHY BANK
      LTPHY = GZTPHY()
C
C--   CHECK LTPHY
      IF ( LTPHY .LE. 0 ) THEN
        CALL ERRMSG('TPHY-NOT-FOUND','GZTND2',
     &    'TPHY BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TND2
      GZTND2 = LC(LTPHY-IZTND2)
C
  999 RETURN
      END
