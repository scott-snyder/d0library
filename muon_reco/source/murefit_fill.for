      SUBROUTINE MUREFIT_FILL(IERR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : fill up MFIT and MUON banks after refitting
C-
C-   Inputs  : 
C-   Outputs : IERR = error code (0 = OK)
C-   Controls: 
C-
C-   Created  18-MAR-1993   Darien R. Wood
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER IERR,NMATCH
C----------------------------------------------------------------------
      IERR = 0
C match up new MUOT banks with MUON banks
      CALL MUREFIT_MATCH(IERR,NMATCH)
C make new MFIT banks
      CALL MUREFIT_MUFIT(IERR)
C update MUON banks
      CALL MUREFIT_LINK(IERR)
C
  999 RETURN
      END
