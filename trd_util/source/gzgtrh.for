      INTEGER FUNCTION GZGTRH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to GTRH bank
C-
C-   Returned value  : Link to 1st element of GTRH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  28-DEC-1989   A. Zylberstejn   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZGTRH.LINK/LIST'
      INTEGER GZGHIT,LZGHIT
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZGTRH=0
C
C--   GET LINK TO SUPPORTING ZTRH BANK
      LZGHIT=GZGHIT()
C
C--   CHECK LZGHIT
      IF(LZGHIT.LE.0)CALL BKGHIT(LZGHIT)
C
C--   FIND LINK TO GTRH
      GZGTRH=LQ(LZGHIT-IZGTRH)
C
  999 RETURN
      END

