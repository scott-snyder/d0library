      FUNCTION l2em_ntuple_bgnrun()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets program at begin run
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   8-OCT-1992   James T. McKinley
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL l2em_ntuple_bgnrun
C----------------------------------------------------------------------
      l2em_ntuple_bgnrun = .true.
      CALL l2em_ntuple_reset
      CALL l2em_ntuple_fill_reset
      END
