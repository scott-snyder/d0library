      SUBROUTINE PYTHRA_BEGIN
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Performs PYTHRA initialization
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  29-JAN-1991   Norman A. Graf
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C ****  Initialize ZEBRA
C
      CALL INZBRA
      CALL ISAZEB('O')
C
C ****  Initialize HBOOK
C
      CALL INPAWC
  999 RETURN
      END
