      FUNCTION GZTPDE()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPDE bank
C-
C-   Returned value  : Link to 1st element of TPDE linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:08:19.83  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTPDE
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPDE.LINK'
C----------------------------------------------------------------------
      INTEGER LTPIO,GZTPIO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPDE = 0
C
C--   GET LINK TO SUPPORTING TPIO BANK
      LTPIO = GZTPIO()
C
C--   CHECK LTPIO
      IF ( LTPIO .LE. 0 ) THEN
        CALL ERRMSG('TPIO-NOT-FOUND','GZTPDE',
     &    'TPIO BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TPDE
      GZTPDE = LC(LTPIO-IZTPDE)
C
  999 RETURN
      END
