      SUBROUTINE TDSTDRP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Drop bank tdst
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JUL-1994   A. Zylberstejn
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER LTDST,GZTDST
C----------------------------------------------------------------------
      INCLUDE 'D0$INC:ZEBCOM.INC'
C----------------------------------------------------------------------
C
      LTDST=GZTDST()
      IF(LTDST.NE.0)        CALL MZDROP(IXMAIN,LTDST,'l')
  999 RETURN
      END
