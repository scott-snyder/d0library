      FUNCTION TB90L2_CALOR_HIST_FILL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Fills all booked histos of tb90l2_calor_hist
C-   called from EXM_POST hook of examine2
C-
C-   Returned value  : TRUE
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_calor_hist_fill
      LOGICAL gen_watch, em_watch, had_smete, had_lgeta ! histogram banks
      LOGICAL had_smeta, lyr_watch, eta_align
      LOGICAL firstin
      INTEGER ier
      DATA firstin/.true./
      SAVE firstin,gen_watch,em_watch,had_smeta,had_lgeta,lyr_watch
      SAVE eta_align
C----------------------------------------------------------------------
      tb90l2_calor_hist_fill = .true.
      IF ( firstin ) THEN
        firstin = .false.
        CALL ezpick('TB90L2_CALOR_HIST_RCP')
        CALL ezget('MAIN_WATCH_FLAG',gen_watch,ier)
        CALL ezget('EM_WATCH_FLAG',em_watch,ier)
        CALL ezget('HAD_SM_ETA_FLAG',had_smeta,ier)
        CALL ezget('HAD_LG_ETA_FLAG',had_lgeta,ier)
        CALL ezget('LAYER_WATCH',lyr_watch,ier)
        CALL ezget('ALIGNMENT_WATCH',eta_align,ier)
        CALL ezrset
      ENDIF
C
C ****  Set hbook directory. There is no error checking done here. Assume that
C ****  if it gets this far it has successfully booked histograms.
C
      CALL dhdir('TB90L2_CALOR_HIST_RCP','HBOOK_DIRECTORY',ier,' ')
      IF(gen_watch) CALL tb90l2_calor_hist_gen_fill
      IF(em_watch) CALL tb90l2_calor_hist_em_fill
      IF(had_smeta) CALL tb90l2_calor_hist_sm_eta_fill
      IF(had_lgeta) CALL tb90l2_calor_hist_lrg_eta_fill
      IF(lyr_watch) CALL tb90l2_calor_hist_lyr_dsp_fill
      IF(eta_align) CALL tb90l2_calor_hist_eta_algn_fill
      RETURN
      END
