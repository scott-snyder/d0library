C DEC/CMS REPLACEMENT HISTORY, Element GZISJT.FOR
C *1    30-JAN-1990 18:04:55 SERBAN "get pointer to first ISJT"
C DEC/CMS REPLACEMENT HISTORY, Element GZISJT.FOR
      FUNCTION GZISJT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Find pointer to ISJT bank (jet found in toy calorimeter)
C-   Returned value  : pointer to first bank
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER GZISJT
      INCLUDE 'D0$LINKS:IZISJT.LINK'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER LISAC,GZISAC
C----------------------------------------------------------------------
      GZISJT=0
      LISAC=GZISAC()
      IF(LISAC.GT.0) GZISJT=LQ(LISAC-IZISJT)
  999 RETURN
      END
