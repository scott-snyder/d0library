      INTEGER FUNCTION GZMSCT(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return MSCT bank pointer
C-
C-   Inputs  : I dummy
C-   Outputs : None
C-   Controls: None
C-
C-   Created   6-MAR-1992   Atsushi Taketani
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZMSCT.LINK'
      INTEGER I,LMUHT,GZMUHT
C----------------------------------------------------------------------
      GZMSCT=0
      LMUHT=GZMUHT(0)
      IF(LMUHT.NE.0) THEN
        GZMSCT=LQ(LMUHT-IZMSCT)
      ENDIF
C 
  999 RETURN
      END
