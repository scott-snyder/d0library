      INTEGER FUNCTION GZRECB()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to RECB bank
C-
C-   Returned value  : Link to 1st element of RECB linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  26-JUL-1990 10:27:01.49  Chip Stewart
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECB.LINK/LIST'
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZRECB=0
C
C--   GET LINK TO SUPPORTING LHEADR BANK
C--   CHECK LHEADR
      IF(LHEADR.LE.0)THEN
        CALL ERRMSG('noHEADRbank','GZRECB',
     &    'HEADR BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO RECB
      GZRECB=LQ(LHEADR-IZRECB)
C
  999 RETURN
      END
