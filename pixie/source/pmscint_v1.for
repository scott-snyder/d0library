      SUBROUTINE PMSCINT_V1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plots Muon Scint in View 1 ( Y-Z View )
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-FEB-1994   V. Bhatnagar
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER  IVIEW

      DATA IVIEW/1/
C----------------------------------------------------------------------
C
      CALL PMAXES(IVIEW)
      CALL PMSCINT_DIS(IVIEW)
  999 RETURN
      END
