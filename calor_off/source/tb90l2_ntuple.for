      FUNCTION tb90l2_ntuple()
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
C-   Created  27-JUN-1991   James Richardson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL tb90l2_ntuple
      LOGICAL tb90l2_ntuple_init
      LOGICAL tb90l2_ntuple_reset
      INTEGER tb90l2_get_module
      INTEGER tb90l2_conv_map_coor_phy_coor
      INCLUDE 'd0$params:tb90l2_ntuple.def'
      INCLUDE 'd0$params:tb90l2_modules.def'
      CHARACTER*80 msg                  ! holds error messages
      INTEGER runno                     ! gets last run number
      INTEGER runnum                    ! run number
      INTEGER nv                        ! bank version number
      INTEGER nr                        ! repeat count
      INTEGER nch                       ! number of chanels from caep bank
      LOGICAL firstcall                 ! true at start of each event
      REAL    low_crazy,high_crazy      ! energy limits
      INTEGER ieta,iphi,layer             ! hit coord froom caep bank
      INTEGER bits                      ! bit mask from caep bank
      REAL    sfrac_corr(MAX_LYR_NUM,NUM_OF_MODULES) ! energy corrections
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
      INTEGER i
      LOGICAL no_cut, wide_cut, narrow_cut
      INTEGER eta_limits(2)
      LOGICAL eta_in_limits, do_eta_cut
      LOGICAL do_simple_clustering
      INTEGER p1
      DATA num_of_events/0/
      SAVE num_of_events,low_crazy,high_crazy,sfrac_corr
      SAVE no_cut, wide_cut, narrow_cut, runnum, do_eta_cut, eta_limits
      SAVE do_simple_clustering
      eta_in_limits(p1) =
     &  (p1 .GE. eta_limits(1) .AND. p1 .LE. eta_limits(2))
C----------------------------------------------------------------------
C
C ****  set flag to true, init counters
C
      tb90l2_ntuple = .true.
      firstcall = .true.
      nchannels = 0
      total_energy = 0.
C
C ****  caculate camac and tracking stuff
C
      CALL tb90l2_ntuple_camac
      CALL tb90l2_ntuple_trk_fit
C
C ****  only have one init for either no cut,wide or narrow
C
      IF (no_cut .OR. wide_cut .OR. narrow_cut)
     &          CALL tb90l2_ntuple_calorim_reset
      CALL gtcaep_header(nv,nr,nch,ier)
      IF (ier.NE.0)THEN
        WRITE(msg,1002)ier
        CALL errmsg('BAD CAEP HEAD','TB90L2_NTUPLE',msg,'W')
        RETURN
      ENDIF
C
C ****  loop over channels
C
      DO i=1,nch
        CALL gtcaep(firstcall,ieta,iphi,layer,bits,energy,ichan,ier)
        IF (ier .NE. 0) THEN
          WRITE (msg,1005)ier
          CALL errmsg('GTCAEP','TB90L2_NTUPLE',msg,'W')
        ENDIF
        IF(firstcall)firstcall=.false.
        IF (energy.LT.low_crazy.OR.energy.GT.high_crazy)THEN
          WRITE(msg,1102)energy,ieta,iphi,layer
          CALL errmsg('BAD CAEP CELL','TB90L2_NTUPLE',msg,'W')
          energy = 0.
        ELSEIF ( .NOT.(do_eta_cut)  .OR.  eta_in_limits(ieta) ) THEN
          nchannels = nchannels + 1
          module = tb90l2_get_module(layer,ieta,iphi)   ! get mod/process
          layer_of_module =
     &      tb90l2_conv_map_coor_phy_coor
     &           (module,layer,ieta,iphi,eta,phi)
          energy=energy*sfrac_corr(layer_of_module,module) ! Sampl fract corr
          total_energy = total_energy + energy
          IF ( no_cut ) THEN
            CALL tb90l2_ntuple_calorim
     &          (NO_E_CUT,module,layer_of_module,eta,phi,energy)
          ELSEIF ( narrow_cut ) THEN
            CALL tb90l2_ntuple_calorim
     &          (NARROW_E_CUT,module,layer_of_module,eta,phi,energy)
          ELSEIF ( wide_cut ) THEN
            CALL tb90l2_ntuple_calorim
     &          (WIDE_E_CUT,module,layer_of_module,eta,phi,energy)
          ENDIF
        ENDIF
      ENDDO
      IF ( do_simple_clustering)
     &      CALL tb90l2_ntuple_simple_clustering
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
 1301 FORMAT(' tb90l2_ntuple > ',i5,' EVENTS, ',
     &    ' RUN ',i7,', TOTAL ENERGY SUM =' ,f10.2)
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_init
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : inits variable for t9b0l2_ntuple
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL ezpick('TB90L2_NTUPLE_RCP')
      CALL ezget('LOW_ENERGY_CRAZY_LIMIT',low_crazy,ier)
      CALL ezget('HIGH_ENERGY_CRAZY_LIMIT',high_crazy,ier)
      CALL ezget('SAMPLING_FRACTION_CORR',sfrac_corr,ier)
      CALL ezget('NTUPLE_NO_CUT',no_cut,ier)
      CALL ezget('NTUPLE_WIDE_CUT',wide_cut,ier)
      CALL ezget('NTUPLE_NARROW_CUT',narrow_cut,ier)
      CALL ezget('DO_GLOBAL_ETA_CUT',do_eta_cut,ier)
      IF ( do_eta_cut ) CALL ezget('GLOBAL_ETA_LIMITS',eta_limits,ier)
      CALL ezget('DO_SIMPLE_CLUSTERING',do_simple_clustering,ier)
      IF ( ier .NE. 0 ) do_simple_clustering = .FALSE.  ! default
      CALL ezrset
      RETURN
C#######################################################################
      ENTRY tb90l2_ntuple_reset
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : reset first to true to reade for next run
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-JUL-1991   James Richardson
C-
C----------------------------------------------------------------------
      CALL tb90l2_get_eta_phi('TB90L2_NTUPLE_RCP',eta,phi)
      CALL tb90l2_ntuple_book
      runnum=runno()
      num_of_events = 1
      END
