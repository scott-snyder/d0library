      FUNCTION GZTDEN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TDEN bank
C-
C-   Returned value  : Link to 1st element of TDEN linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   20-FEB-1996 Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTDEN
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTDEN.LINK'
C----------------------------------------------------------------------
      INTEGER GZTGEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTDEN = 0
C
C--   GET LINK TO SUPPORTING TGEN BANK
C      LTGEN = GZTGEN()
C
C--   CHECK LTGEN
      IF ( LTGEN .LE. 0 ) THEN
        CALL ERRMSG('TGEN-NOT-FOUND','GZTDEN',
     &    'TGEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TDEN
      GZTDEN = LC(LTGEN-IZTDEN)
C
  999 RETURN
      END
