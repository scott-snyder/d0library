      FUNCTION tb90l2_calor_hist()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :Processes events
C-   Called from EXM_DO_ANALYSIS hook of d0$examine2
C-
C-   Returned value  : true
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  14-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_calor_hist
      LOGICAL tb90l2_Calor_hist_init
      LOGICAL tb90l2_Calor_hist_reset
      INTEGER tb90l2_get_module
      INTEGER tb90l2_conv_map_coor_phy_coor
      INCLUDE 'd0$params:tb90l2_modules.def'
      CHARACTER*80 msg                  ! holds error messages
      INTEGER runno                     ! gets last run number
      INTEGER runnum                    ! run number
      INTEGER nv                        ! bank version number
      INTEGER nr                        ! repeat count
      INTEGER nch                       ! number of chanels from caep bank
      LOGICAL first                     ! true if first time in
      LOGICAL firstcall                 ! true at start of each event
      REAL    low_crazy,high_crazy      ! energy limits
      INTEGER ieta,iphi,layer             ! hit coord froom caep bank
      INTEGER bits                      ! bit mask from caep bank
      REAL    sfrac_corr(max_lyr_num,num_of_modules) ! energy corrections
      REAL    energy                    ! energy
      REAL    total_energy              ! running sum of energy
      INTEGER ichan                     ! channel number
      INTEGER nchannels                 ! good channels
      INTEGER num_of_events             ! total number of events
      INTEGER module                    ! module hit occurred in
      INTEGER layer_of_module           ! physical layer of module
      REAL    eta,phi                   ! coordinates of readout pad.
      ! Account for divisions of EM3
      INTEGER ier                       ! error flag
      INTEGER i, p1
      LOGICAL gen_watch, em_watch, had_smete, had_lgeta ! histogram banks
      LOGICAL had_smeta, lyr_watch, eta_align, lego_plots
      INTEGER eta_limits(2)
      LOGICAL eta_in_limits, do_eta_cut
      DATA first/.true./
      SAVE num_of_events,first,low_crazy,high_crazy,sfrac_corr
      SAVE gen_watch,em_watch,had_smeta,had_lgeta,lyr_watch,eta_align
      SAVE lego_plots, runnum, do_eta_cut, eta_limits
      eta_in_limits(p1) =
     &  (p1 .GE. eta_limits(1) .AND. p1 .LE. eta_limits(2))
C----------------------------------------------------------------------
C
C ****  get eta,phi only on first event
C
      IF (first) THEN
        CALL tb90l2_get_eta_phi('TB90L2_CALOR_HIST_RCP',eta,phi)
        first = .false.
        runnum=runno()
      ENDIF
C
C ****  set flag to true, init counters and get event header
C
      tb90l2_calor_hist = .true.
      firstcall = .true.
      nchannels = 0
      total_energy = 0.
      IF(gen_watch) CALL tb90l2_calor_hist_gen_init
      IF(em_watch) CALL tb90l2_calor_hist_em_init
      IF(had_smeta) CALL tb90l2_calor_hist_sm_eta_init
      IF(had_lgeta) CALL tb90l2_calor_hist_lrg_eta_init
      IF(lyr_watch) CALL tb90l2_calor_hist_lyr_dsp_init
      IF(eta_align) CALL tb90l2_calor_hist_eta_algn_init
      CALL gtcaep_header(nv,nr,nch,ier)
      IF (ier.NE.0)THEN
        WRITE(msg,1002)ier
        CALL errmsg('BAD CAEP HEAD','TB90L2_CALOR_HIST',msg,'W')
        RETURN
      ENDIF
      CALL dhdir('TB90L2_CALOR_HIST_RCP','HBOOK_DIRECTORY',ier,' ')
      CALL tb90l2_calor_hist_gen_chan(nch)
C
C ****  loop over channels
C
      DO i=1,nch
        CALL gtcaep(firstcall,ieta,iphi,layer,bits,energy,ichan,ier)
        IF (ier .NE. 0) THEN
          WRITE (msg,1005)ier
          CALL errmsg('GTCAEP','TB90L2_CALOR_HIST',msg,'W')
        ENDIF
        IF(firstcall)firstcall=.false.
        IF (energy.LT.low_crazy.OR.energy.GT.high_crazy)THEN
          WRITE(msg,1102)energy,ieta,iphi,layer
          CALL errmsg('BAD CAEP CELL','TB90L2_CALOR_HIST',msg,'W')
          energy = 0.
        ELSEIF ( .NOT.(do_eta_cut)  .OR.  eta_in_limits(ieta) ) THEN
          nchannels = nchannels + 1
          module = tb90l2_get_module(layer,ieta,iphi)   ! get mod/process
          layer_of_module = tb90l2_conv_map_coor_phy_coor(module,layer,
     &      ieta,iphi,eta,phi)
          energy=energy*sfrac_corr(layer_of_module,module) ! Sampl fract corr
          total_energy = total_energy + energy
          IF(gen_watch) CALL
     &      tb90l2_calor_hist_gen
     &         (module,layer,layer_of_module,eta,phi,energy)
          IF(em_watch) CALL
     &      tb90l2_calor_hist_em(module,layer_of_module,eta,phi,energy)
          IF(had_smeta) CALL
     &      tb90l2_calor_hist_sm_eta
     &         (module,layer_of_module,eta,phi,energy)
          IF(had_lgeta) CALL
     &      tb90l2_calor_hist_lrg_eta
     &         (module,layer_of_module,eta,phi,energy)
          IF(lyr_watch) CALL
     &      tb90l2_calor_hist_lyr_dsp(module,layer_of_module,energy)
          IF(eta_align) CALL
     &      tb90l2_calor_hist_eta_algn
     &         (module,layer_of_module,eta,phi,energy)
          IF ( lego_plots) CALL
     &      tb90l2_calor_hist_lego
     &         (module,layer_of_module,eta,phi,energy)
        ENDIF
      ENDDO
      IF(mod(num_of_events,10).eq.0) then
        WRITE(msg,1301)num_of_events,runnum,total_energy
        CALL intmsg(msg)
      END IF
      num_of_events = num_of_events + 1
C
C ****  Format statements
C
 1002 FORMAT(' IER in GTCAEP_HEADER=',i3)
 1005 FORMAT(' IER in GTCAEP = ',i3)
 1102 FORMAT('CRAZY ENERGY, CHANNEL ZEROED ', f10.1,3i4)
 1301 FORMAT(' TB90L2_CALOR_HIST > ',i5,' EVENTS, ',
     &    ' RUN ',i7,', TOTAL ENERGY SUM =' ,f10.2)
      RETURN
C#######################################################################
      ENTRY tb90l2_calor_hist_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : initializes variables for tb90l2_calor_hist
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
C
C ****  fetch energy limits from rcp bank, get runnum
C
      CALL ezpick('TB90L2_CALOR_HIST_RCP')
      CALL ezget('LOW_ENERGY_CRAZY_LIMIT',low_crazy,ier)
      CALL ezget('HIGH_ENERGY_CRAZY_LIMIT',high_crazy,ier)
      CALL ezget_rarr('SAMPLING_FRACTION_CORR',sfrac_corr,ier)
      CALL ezget_l('MAIN_WATCH_FLAG',gen_watch,ier)
      CALL ezget_l('EM_WATCH_FLAG',em_watch,ier)
      CALL ezget_l('HAD_SM_ETA_FLAG',had_smeta,ier)
      CALL ezget_l('HAD_LG_ETA_FLAG',had_lgeta,ier)
      CALL ezget_l('LAYER_WATCH',lyr_watch,ier)
      CALL ezget_l('ALIGNMENT_WATCH',eta_align,ier)
      CALL ezget_l('DO_LEGO',lego_plots,ier)
      CALL ezget_l('DO_GLOBAL_ETA_CUT',do_eta_cut,ier)
      IF ( do_eta_cut ) CALL ezget_iarr('GLOBAL_ETA_LIMITS',eta_limits,
     &     ier)
      
      CALL ezrset
      RETURN
C#######################################################################
      ENTRY tb90l2_Calor_hist_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : resets first to true to ready for next run
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  23-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      first = .true.
      num_of_events = 1
      RETURN
      END
