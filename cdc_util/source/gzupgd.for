      INTEGER FUNCTION GZUPGD()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  23-OCT-1995   Hailin Li
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZUPGD.LINK'
      INTEGER LHITS,GZHITS
C----------------------------------------------------------------------
      GZUPGD = 0
C
      LHITS = GZHITS()
      IF ( LHITS .LE. 0 ) RETURN
      GZUPGD = LQ(LHITS-IZUPGD)
  999 RETURN
      END
