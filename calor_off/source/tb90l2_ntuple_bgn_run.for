      FUNCTION tb90l2_ntuple_bgn_run()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Resets counter at begin run
C-   Called from the EXM_BEGIN_ANALYSIS hook of examine2
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  27-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_ntuple_bgn_run
C----------------------------------------------------------------------
      tb90l2_ntuple_bgn_run = .true.
      CALL tb90l2_ntuple_fill_reset
      CALL tb90l2_ntuple_reset
      END
