      INTEGER FUNCTION GZGSWTZ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS LINK OF GSWT BANK
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-SEP-1989   A.M.Jonckheere
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER GZD0RGZ,LD0RG
      INCLUDE 'D0$LINKS:IZGSWT.LINK'
C----------------------------------------------------------------------
      GZGSWTZ = 0
      LD0RG = GZD0RGZ()
      IF ( LD0RG.GT.0 ) GZGSWTZ = LQ(LD0RG-IZGSWT)
  999 RETURN
      END
