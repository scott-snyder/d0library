      FUNCTION TB90L2_CALOR_HIST_BGN_RUN()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : sets up begin run stuff
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_calor_hist_bgn_run
      INTEGER ier
C----------------------------------------------------------------------
      tb90l2_calor_hist_bgn_run = .true.
      CALL ezpick('TB90L2_CALOR_HIST_RCP')
      CALL dhdir('TB90L2_CALOR_HIST_RCP','HBOOK_DIRECTORY',ier,' ')
      CALL hreset(0,' ')
      CALL tb90l2_calor_hist_reset
      END
