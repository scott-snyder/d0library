      INTEGER FUNCTION GZCAEP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to CAEP 
C-
C-   Created  13-JAN-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAEP.LINK'
      INTEGER GZCAHT,LCAHT,LCAEP
C----------------------------------------------------------------------
C
      LCAHT=GZCAHT()
      LCAEP=0
      IF(LCAHT.NE.0)  LCAEP=LQ(LCAHT-IZCAEP)
      GZCAEP=LCAEP
C
      RETURN
      END
