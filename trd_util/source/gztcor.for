      FUNCTION GZTCOR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TCOR bank
C-
C-   Returned value  : Link to 1st element of TCOR linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-FEB-1996   A. ZYLBERSTEJN   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTCOR
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTCOR.LINK'
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTCOR = 0
C
C
C--   CHECK LTGEN
      IF ( LTGEN .LE. 0 ) THEN
        CALL ERRMSG('TGEN-NOT-FOUND','GZTCOR',
     &    'TGEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TCOR
      GZTCOR = LC(LTGEN-IZTCOR)
C
  999 RETURN
      END
