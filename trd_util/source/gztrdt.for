      INTEGER FUNCTION GZTRDT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TRDT bank
C-
C-   Returned value  : Link to 1st element of TRDT linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  20-OCT-1989 18:36:53.08  A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRDT.LINK/LIST'
      INTEGER LTTRH,GZTTRH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTRDT=0
C
C--   GET LINK TO SUPPORTING TTRH BANK
      LTTRH=GZTTRH()
C
C--   CHECK LTTRH
      IF(LTTRH.LE.0)THEN
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TRDT
      GZTRDT=LQ(LTTRH-IZTRDT)
C
  999 RETURN
      END

