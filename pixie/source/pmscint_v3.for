      SUBROUTINE PMSCINT_V3
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plots Muon Scint in View 3 ( Z-X View )
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-FEB-1994   V. Bhatnagar
C-   Modified 02-MAR-1994   V.Bhatnagar axes view changed to 6 (Z_X)
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IVIEW

      DATA IVIEW/3/
C----------------------------------------------------------------------
C
      CALL PMAXES(6)
      CALL PMSCINT_DIS(IVIEW)
  999 RETURN
      END
