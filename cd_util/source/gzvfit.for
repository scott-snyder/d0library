      FUNCTION GZVFIT(IVFIT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return zebra pointer to bank VFIT
C-
C-   Returned value  :
C-   Inputs  : IVFIT = Vertex number whose information is desired
C-   Outputs : Address of IVFIT bank
C-   Controls: none
C-
C-   Created  19-NOV-1993   Srini Rajagopalan
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZVFIT.LINK'
C
      INTEGER IVFIT,LVERH,LVFIT
      INTEGER GZVFIT,GZVERH,LZFIND
C----------------------------------------------------------------------
      GZVFIT=0
      LVERH=GZVERH()
      IF (LVERH.GT.0) THEN
        LVFIT=LQ(LVERH-IZVFIT)
        IF (LVFIT.GT.0) GZVFIT=LZFIND(IXCOM,LVFIT,IVFIT,-5)
      END IF
C
  999 RETURN
      END
