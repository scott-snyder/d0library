      FUNCTION GZTDPI()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TDPI bank
C-
C-   Returned value  : Link to 1st element of TDPI linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   19-FEB-1996 08:52:58.90  Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTDPI
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTDPI.LINK'
C----------------------------------------------------------------------
      INTEGER LTDEN,GZTDEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTDPI = 0
C
C--   GET LINK TO SUPPORTING TDEN BANK
      LTDEN = GZTDEN()
C
C--   CHECK LTDEN
      IF ( LTDEN .LE. 0 ) THEN
        CALL ERRMSG('TDEN-NOT-FOUND','GZTDPI',
     &    'TDEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TDPI
      GZTDPI = LC(LTDEN-IZTDPI)
C
  999 RETURN
      END
