      INTEGER FUNCTION GZTPHY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TPHY bank
C-
C-   Returned value  : Link to 1st element of TPHY 
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBSTP.INC'
      INCLUDE 'D0$LINKS:IZTPHY.LINK/LIST'
      INTEGER GZTGEN
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTPHY=0
C
C
C--   CHECK LTGEN
      IF(LTGEN.LE.0)THEN
        CALL ERRMSG('GZTPHY:TGEN BANK DOES NOT EXIST ' ,'W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TPHY
      GZTPHY=LC(LTGEN-IZTPHY)
C
  999 RETURN
      END

