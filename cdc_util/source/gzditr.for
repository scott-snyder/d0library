      INTEGER FUNCTION GZDITR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns pointer to Zebra bank DITR
C-
C-   Returned value  : GZDITR = pointer to Zebra bank DITR
C-
C-   Created  17-AUG-1991   Tom Trippe
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LDTRH, GZDTRH
      INCLUDE 'D0$LINKS:IZDITR.LINK/LIST'
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'                             
C----------------------------------------------------------------------
      GZDITR=0
      LDTRH = GZDTRH(0)
      IF (LDTRH.NE.0) GZDITR=LQ(LDTRH-IZDITR) 
  999 RETURN
      END
