      INTEGER FUNCTION GZTRGH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : return pointer to TRGH bank
C-
C-                                                     Marcel Demarteau
C-                                                         31 Oct. 1987
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZRECO.LINK'
      INCLUDE 'D0$LINKS:IZHITS.LINK'
      INCLUDE 'D0$LINKS:IZTRGH.LINK'
      INTEGER LRECO,LHITS
C----------------------------------------------------------------------
C
      GZTRGH=0
      LRECO=LQ(LHEAD-IZRECO)
      IF(LRECO.NE.0) THEN
        LHITS=LQ(LRECO-IZHITS)
        IF(LHITS.NE.0) GZTRGH=LQ(LHITS-IZTRGH)
      ENDIF
C
      RETURN
      END
