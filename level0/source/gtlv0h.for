      SUBROUTINE GTLV0H(CBUNCH,NBUNCH)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fetch contents of Zebra bank LV0H
C-
C-   Inputs  : None
C-   Outputs : CBUNCH = correct bunch in the Level 0 detector data
C-             NBUNCH = number of bunches stored
C-   Controls: None
C-
C-   Created  13-JUL-1992   Freedy Nang
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
C
      INTEGER CBUNCH
      INTEGER NBUNCH
      INTEGER LLV0H
      INTEGER GZLV0H
      EXTERNAL GZLV0H
C----------------------------------------------------------------------
      CBUNCH=0
      NBUNCH=0
      LLV0H=GZLV0H()
      IF (LLV0H.LE.0) GOTO 999
      CBUNCH=IQ(LLV0H+1)
      NBUNCH=IQ(LLV0H+2)
C----------------------------------------------------------------------
  999 RETURN
      END
