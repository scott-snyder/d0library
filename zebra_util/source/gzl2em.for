      INTEGER FUNCTION GZL2EM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Returns the Link to L2EM bank
C-
C-   Returned value  : Link to 1st element of L2EM
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  4-NOV-1991, Yi  Xia
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZL2EM.LINK/LIST'
      INTEGER LFRES,GZFRES
C----------------------------------------------------------------------
C
C--   INITIALIZE
      GZL2EM = 0
C
C--   GET LINK TO SUPPORTING FRES BANK
      LFRES = GZFRES()
C
      IF(LFRES.NE.0) GZL2EM = LQ(LFRES - IZL2EM)
C
  999 RETURN
      END
