      INTEGER FUNCTION GZCAEH
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to CAEH 
C-
C-   Created  13-JAN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEH.LINK'
      INTEGER GZCAHT,LCAHT,LCAEH
C----------------------------------------------------------------------
C
      LCAHT=GZCAHT()
      LCAEH=0
      IF(LCAHT.NE.0)  LCAEH=LQ(LCAHT-IZCAEH)
      GZCAEH=LCAEH
C
      RETURN
      END
