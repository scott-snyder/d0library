      INTEGER FUNCTION GZSAPH(I)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get location of bank 'SAPH'.
C-
C-   Inputs  : 
C-      I       I   dummy
C-
C-   Outputs : 
C-      GZSAPH  I   address of bank, SAPH.
C-
C-   Controls: 
C-
C-   Created  22-AUG-1994   M. Fortner
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZSAPH.LINK'
      INTEGER I,LMUHT,GZMUHT
      GZSAPH=0
      LMUHT=GZMUHT(0)
      IF(LMUHT.NE.0) THEN
        GZSAPH=LQ(LMUHT-IZSAPH)
      ENDIF
      RETURN
      END
