      SUBROUTINE VTX_CLRLNK_VSEC(LAY,SEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Clear the links for a given sector
C-
C-   Inputs  : LAY - Layer , SEC - sector whose link to be cleared
C-
C-   Created   5-JUL-1994   Danilo Puseljic
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:VTXLNK.INC'
C
      INTEGER LAY, SEC
C
C----------------------------------------------------------------------
C
      LVSEC(SEC,LAY) = 0
      LVWDA(SEC,LAY) = 0
C
  999 RETURN
      END
