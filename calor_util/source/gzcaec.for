      FUNCTION GZCAEC
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      return link to CAEC bank
C-   Returned value  : link
C-
C-   Created  20-MAR-1990   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEC.LINK'
      INTEGER GZCAEC,GZCAEH
      INTEGER LCAEH
C----------------------------------------------------------------------
C
      GZCAEC=0
      LCAEH=GZCAEH()
      IF(LCAEH.GT.0) GZCAEC=LQ(LCAEH-IZCAEC)
  999 RETURN
      END
