      INTEGER FUNCTION GZTRDH()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to TRDH bank
C-
C-   Returned value  : Link to 1st element of TRDH linear structure
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-OCT-1989 18:42:05.09  A. Zylberstej
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZTRDH.LINK/LIST'
      INTEGER GZHITS,LZHITS,GZZTRH
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZTRDH=0
C
C--   GET LINK TO SUPPORTING ZTRH BANK
      LZHITS=GZHITS()
C
C--   CHECK LZHITS
      IF(LZHITS.LE.0)RETURN
C
C--   FIND LINK TO TRDH
      GZTRDH=LQ(LZHITS-IZTRDH)
C
  999 RETURN
      END

