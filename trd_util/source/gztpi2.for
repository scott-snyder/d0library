      FUNCTION GZTPI2()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPI2 bank
C-
C-   Returned value  : Link to 1st element of TPI2 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:06:39.98  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTPI2
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPI2.LINK'
C----------------------------------------------------------------------
      INTEGER LTPIO,GZTPIO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPI2 = 0
C
C--   GET LINK TO SUPPORTING TPIO BANK
      LTPIO = GZTPIO()
C
C--   CHECK LTPIO
      IF ( LTPIO .LE. 0 ) THEN
        CALL ERRMSG('TPIO-NOT-FOUND','GZTPI2',
     &    'TPIO BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TPI2
      GZTPI2 = LC(LTPIO-IZTPI2)
C
  999 RETURN
      END
