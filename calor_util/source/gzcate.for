      INTEGER FUNCTION GZCATE
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-     Find pointer to CATE 
C-
C-   Created  03-MAR-1989   Andrzej Zieminski
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCATE.LINK'
      INTEGER GZCAHT,LCAHT,LCATE
C----------------------------------------------------------------------
C
      LCAHT=GZCAHT()
      LCATE=0
      IF(LCAHT.NE.0)  LCATE=LQ(LCAHT-IZCATE)
      GZCATE=LCATE
C
      RETURN
      END
