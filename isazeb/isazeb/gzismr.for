      FUNCTION GZISMR()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     get pointer to ISMR bank
C-   Returned value  : pointer value
C-
C-   Created  19-MAY-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISMR
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISMR.LINK'
      INTEGER LISAC,GZISAC
C----------------------------------------------------------------------
C
      LISAC=GZISAC()
      GZISMR=0
      IF(LISAC.NE.0) GZISMR=LQ(LISAC-IZISMR)
  999 RETURN
      END
