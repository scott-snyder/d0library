      FUNCTION GZGSWT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : GETS LINK OF GSWT BANK
C-
C-   Returned value  : 
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  17-JUL-1989   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:GCBANK.INC'
      INCLUDE 'D0$INC:GCLINK.INC'
      INTEGER GZD0RG,LD0RG,GZGSWT
      INCLUDE 'D0$LINKS:IZGSWT.LINK'
C----------------------------------------------------------------------
      GZGSWT = 0
      LD0RG = GZD0RG()
      IF ( LD0RG.GT.0 ) GZGSWT = LQ(LD0RG-IZGSWT)
  999 RETURN
      END
