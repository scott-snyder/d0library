      FUNCTION GB_TRIGGER_EVT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Global Monitor Trigger event processor
C-
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created  20-NOV-1992   Jeffrey Bantly
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL GB_TRIGGER_EVT
      LOGICAL OK
      LOGICAL GB_TRIGGER_HIST
      EXTERNAL GB_TRIGGER_HIST
C----------------------------------------------------------------------
      OK=GB_TRIGGER_HIST()
      GB_TRIGGER_EVT = OK
C----------------------------------------------------------------------
  999 RETURN
      END
