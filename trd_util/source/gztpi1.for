      FUNCTION GZTPI1()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPI1 bank
C-
C-   Returned value  : Link to 1st element of TPI1 linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   6-SEP-1994 09:04:14.76  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTPI1
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPI1.LINK'
C----------------------------------------------------------------------
      INTEGER LTPIO,GZTPIO
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPI1 = 0
C
C--   GET LINK TO SUPPORTING TPIO BANK
      LTPIO = GZTPIO()
C
C--   CHECK LTPIO
      IF ( LTPIO .LE. 0 ) THEN
        CALL ERRMSG('TPIO-NOT-FOUND','GZTPI1',
     &    'TPIO BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TPI1
      GZTPI1 = LC(LTPIO-IZTPI1)
C
  999 RETURN
      END
