      SUBROUTINE CENP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : CALL SUBROUTINES FOR ENDPLATES
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  19-SEP-1988   Stephen Kahn, Esq.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C----------------------------------------------------------------------
      CALL CENPCC             ! creates CEN CAL end plate banks
C
      CALL CENPEH             ! creates END CAL end plate banks
C
  999 RETURN
      END
