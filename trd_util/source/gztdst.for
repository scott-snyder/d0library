      INTEGER FUNCTION GZTDST()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to tdst bank
C-
C-   Returned value  : Link to 1st element of tdst linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JUN-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTDST,LZFIDH,NTDST,TDST
      integer LTTRH,GZTTRH
C----------------------------------------------------------------------
      INCLUDE 'D0$LINKS:IZTDST.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTDST = 0
C
C--   GET LINK TO SUPPORTING TTRH BANK
      LTTRH=GZTTRH()
C
C--   CHECK LTTRH
      IF(LTTRH.LE.0)THEN
        CALL ERRMSG('LTTRH does not exist','GZTDST',' ','W')
        GO TO 999
      ENDIF
C
C--   FIND LINK TO TDST
      GZTDST=LQ(LTTRH-IZTDST)
C
C
  999 RETURN
      END
