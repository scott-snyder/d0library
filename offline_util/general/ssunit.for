      INTEGER FUNCTION SSUNIT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     return output unit number for Standard Output
C-
C-   ENTRY SSUNIT
C-   Inputs  : 
C-   OUT=  output unit number
C-
C-   Created  21-MAR-1988   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER STSSUN,OUT,OUNIT
C----------------------------------------------------------------------
C
      SSUNIT=OUNIT
      RETURN
C
      ENTRY STSSUN(OUT)
      OUNIT=OUT
  999 RETURN
      END
