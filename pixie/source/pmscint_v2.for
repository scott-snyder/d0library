      SUBROUTINE PMSCINT_V2
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Plots Muon Scint in View 2 ( X-Y View )
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

      DATA IVIEW/2/
C----------------------------------------------------------------------
C
      CALL PMAXES(IVIEW)
      CALL PMSCINT_DIS(IVIEW)
  999 RETURN
      END
