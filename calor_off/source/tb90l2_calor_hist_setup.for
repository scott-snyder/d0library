      FUNCTION TB90L2_CALOR_HIST_SETUP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Reads in tb90l2_calor_hist_rcp file. Books histos
C-   and does initialization associated with historgrams.
C-   To be called from EXM_SETUP hook of examine2
C-
C-   Returned value  : TRUE if found RCP file
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_calor_hist_setup
      LOGICAL ezerr
      LOGICAL gen_watch, em_watch, had_smete, had_lgeta ! histogram banks
      LOGICAL had_smeta, lyr_watch, eta_align, lego_plots, lego
      INTEGER ier
C----------------------------------------------------------------------
      tb90l2_calor_hist_setup = .true.
C
C ****  read in rcp file, book historgram
C
      CALL inrcp('TB90L2_CALOR_HIST_RCP',ier)
      IF (ier.EQ.0) THEN
        CALL ezpick('TB90L2_CALOR_HIST_RCP')
        IF(ezerr(ier)) THEN
          CALL errmsg('INRCP-NO BANK','TB90L2_CALOR_HIST_SETUP',
     &    ' COULD NOT FIND TB90L2_CALOR_HIST_RCP BANK ','F')
        ENDIF
        CALL dhdir('TB90L2_CALOR_HIST_RCP','HBOOK_DIRECTORY',ier,' ')
        CALL ezget('MAIN_WATCH_FLAG',gen_watch,ier)
        CALL ezget('EM_WATCH_FLAG',em_watch,ier)
        CALL ezget('HAD_SM_ETA_FLAG',had_smeta,ier)
        CALL ezget('HAD_LG_ETA_FLAG',had_lgeta,ier)
        CALL ezget('LAYER_WATCH',lyr_watch,ier)
        CALL ezget('ALIGNMENT_WATCH',eta_align,ier)
        CALL ezget('DO_LEGO',lego_plots,ier)
        IF(gen_watch)CALL do_hbook('MAIN_HISTOGRAMS')
        IF(em_watch)CALL do_hbook('EM_HISTOGRAMS')
        IF(had_smeta)CALL do_hbook('SMALL_ETA_HISTOGRAMS')
        IF(had_lgeta)CALL do_hbook('LARGE_ETA_HISTOGRAMS')
        IF(lyr_watch)CALL do_hbook('LAYER_DISPLAY_HISTOGRAMS')
        IF(eta_align)CALL do_hbook('ETA_IN_LAYERS_HISTOGRAMS')
        IF(lego_plots)CALL do_hbook('LEGO_PLOTS')
        CALL ezrset
        CALL tb90l2_calor_hist_init
      ELSE
        CALL errmsg('INRCP-NO RCP','TB90L2_CALOR_HIST_SETUP',
     &       'NO TB90L2_CALOR_HIST_RCP','W')
        CALL errmsg('INRCP','TB90L2_CALOR_HIST_SETUP',
     &    'CANT CONTINUE without TB90L2_CALOR_HIST_RCP','F')
        tb90l2_calor_hist_setup = .false.
      END IF
      RETURN
      END
