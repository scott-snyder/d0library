      INTEGER FUNCTION GZTTRH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TTRH bank
C-
C-   Returned value  : Link to 1st element of TTRH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-OCT-1989 18:42:05.09  A. Zylberstej
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTTRH.LINK/LIST'
      INTEGER LZTRH,GZZTRH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTTRH=0
C
C--   GET LINK TO SUPPORTING ZTRH BANK
      LZTRH=GZZTRH()
C
C--   CHECK LZTRH
      IF(LZTRH.LE.0)THEN
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TTRH
      GZTTRH=LQ(LZTRH-IZTTRH)
C
  999 RETURN
      END

