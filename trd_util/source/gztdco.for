      FUNCTION GZTDCO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TDCO bank
C-
C-   Returned value  : Link to 1st element of TDCO linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created   19-FEB-1996 08:52:58.90  Y. Ducros
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZTDCO
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTDCO.LINK'
C----------------------------------------------------------------------
      INTEGER GZTDEN,LTDEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTDCO = 0
C
C--   GET LINK TO SUPPORTING TDEN BANK
      LTDEN = GZTDEN()
C
C--   CHECK LTDEN
      IF ( LTDEN .LE. 0 ) THEN
        CALL ERRMSG('TDEN-NOT-FOUND','GZTDCO',
     &    'TDEN BANK DOES NOT EXIST ' ,'W')
        GOTO 999
      ENDIF
C
C--   FIND LINK TO TDCO
      GZTDCO = LC(LTDEN-IZTDCO)
C
  999 RETURN
      END
