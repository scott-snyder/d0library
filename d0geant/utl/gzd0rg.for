      FUNCTION GZD0RG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RETURNS LINK OF D0RG
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
      INTEGER GZD0RG
      INCLUDE 'D0$LINKS:IZD0RG.LINK'
C----------------------------------------------------------------------
      GZD0RG = 0
      IF(JRUNG.EQ.0)GO TO 999
      GZD0RG = LQ(JRUNG-IZD0RG)
  999 RETURN
      END
