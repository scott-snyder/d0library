      FUNCTION GZTDEL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TDEL bank
C-
C-   Returned value  : Link to 1st element of TDEL linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   19-FEB-1996 08:52:58.90  Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTDEL
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTDEL.LINK'
C----------------------------------------------------------------------
      INTEGER GZTDEN,LTDEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTDEL = 0
C
C--   GET LINK TO SUPPORTING TDEN BANK
      LTDEN = GZTDEN()
C
C--   CHECK LTDEN
      IF ( LTDEN .LE. 0 ) THEN
        CALL ERRMSG('TDEN-NOT-FOUND','GZTDEL',
     &    'TDEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TDEL
      GZTDEL = LC(LTDEN-IZTDEL)
C
  999 RETURN
      END
