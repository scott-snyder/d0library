      FUNCTION GZTANA(layer)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TANA bank
C-
C-   Returned value  : Link to 1st element of TANA linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  XDATE  XAUTHOR
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTANA,layer
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C      INCLUDE 'D0$LINKS:IZTANA.LINK'
C----------------------------------------------------------------------
      INTEGER LTDST,GZTDST
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTANA = 0
C
C--   GET LINK TO SUPPORTING tdst BANK
      LTDST = GZTDST()
C
C--   CHECK Ltdst
      IF ( LTDST .LE. 0 ) THEN
        CALL ERRMSG('tdst-NOT-FOUND','GZTANA',
     &    'tdst BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TANA
      GZTANA = LQ(LTDST-LAYER)
C
  999 RETURN
      END
