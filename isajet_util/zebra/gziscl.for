C DEC/CMS REPLACEMENT HISTORY, Element GZISCL.FOR
C *1    30-JAN-1990 18:04:16 SERBAN "get pointer to first ISCL"
C DEC/CMS REPLACEMENT HISTORY, Element GZISCL.FOR
      FUNCTION GZISCL()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     get pointer to first ISCL bank
C-   Returned value  :  pointer
C-
C-   Created  29-JAN-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISAC.LINK'
      INCLUDE 'D0$LINKS:IZISCL.LINK'
      INTEGER GZISCL,GZISAC,LISAC
C----------------------------------------------------------------------
      GZISCL=0
      LISAC=GZISAC()
      IF (LISAC.NE.0  ) GZISCL=LQ(LISAC-IZISCL)
  999 RETURN
      END
