      INTEGER FUNCTION GZCAHT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-      return pointer to CAHT
C-
C-   Created  30-SEP-1987   Serban D. Protopopescu
C-   Updated  10-FEB-1989   Alan M. Jonckheere  - Corrected to work under GEAN
C-                                          as well as RECO - uses new GZHITS
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAHT.LINK'
      INTEGER LHITS,GZHITS
C----------------------------------------------------------------------
C
      GZCAHT=0
      LHITS=GZHITS()
      IF(LHITS.NE.0) GZCAHT=LQ(LHITS-IZCAHT)
C
  999 RETURN
      END
