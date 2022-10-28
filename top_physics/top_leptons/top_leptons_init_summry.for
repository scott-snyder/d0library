      SUBROUTINE TOP_LEPTONS_INIT_SUMMRY
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print a summary of run cuts/initialization
C-                                 on LUN=LEPTONS_LUNSUM
C-                          (if wanted => PRINT_LEPSUM =.TRUE.)
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  28-FEB-1992   Stephen J. Wimpenny
C-   Modified 10-JUL-1992   to conform to Serban's format
C-   Modified 14-JUL-1992   i/o change + LUN bug fixed
C-   Modified  3-AUG-1992   Ntuple,W-finder,Histogram and Electron
C-                          quality parameters added
C-   Modified 14-Sep-1992   Parameters read directly from RCP file
C-   Modified 18-Sep-1992   Additional muon cuts added
C-   Modified 25-Sep-1992   W-finder specific cuts added
C-   Modified 18-Oct-1992   Modifed Electron finder cuts and new top
C-                          switches added
C-   Modified 23-Oct-1992   Bdl and IFW1 cuts added
C-   Modified  1-Nov-1992   Vertex and trigger cuts added
C-   Modified  2-Nov-1992   Bad Run Rejection added
C-   Modified 13-Nov-1992   Run range checking added
C-   Modified 16-Nov-1992   e+jets,mu+jets hooks added
C-   Modified 14-Dec-1992   e/ph/mu vs jet isolation cuts added
C-   Modified 30-Dec-1992   mass cuts added
C-   Modified 26-Jan-1993   micro_blank added
C-   Modified  1-Feb-1993   MRBS loss veto added
C-   Modified  3-Feb-1993   Separate calls to CLEANEM and CLEANMU init
C-                          summary routines for e/gam and muon id
C-   Modified 17-Mar-1993   Changes in routine names for CLEANEM and
C-                          CLEANMU init summries
C-   Modified 30-Mar-1993   Mt window cut added to remove winde angle
C-                          bremm events
C-   Modified  7-Apr-1993   Added soft muon and primary mu iso cuts 
C-                          for find mujet
C-   Modified 22-Apr-1993   RECO diagnostics logical + mujet updates
C-                          added
C-   Modified 16-May-1993   New Dilepton Isolation Code added
C-                          WGamma and Wpair code mods added
C-   Modified 16-Jul-1993   Energy correction code added
C-                          Muon dE/dx correction added
C-   Modified  4-Dec-1993   Filter Select and dumps for new version
C-                          of FIND_MUJET added
C-   Modified  3-Apr-1994   New features added
C-   Updated  23-MAR-2004   sss - compile with g77.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL PRINT_LEPSUM,DUMP_EVENT,LONG_EVENT_DUMP
      LOGICAL CORR_EM,CORR_JETS,CORR_MU
      LOGICAL DO_FINDW,DO_FIND_WE,DO_FIND_WMU
      LOGICAL DO_FINDZ,DO_FIND_ZEE,DO_FIND_ZMUMU
      LOGICAL DO_FINDWPAIR,DO_FINDWPAIR_EE,DO_FINDWPAIR_EMU
      LOGICAL DO_FINDWPAIR_MUMU,DO_FINDWPAIR_MUJET,DO_FINDWPAIR_EJET
      LOGICAL DO_FINDQCD,DO_FINDQCD_EE,DO_FINDQCD_EMU
      LOGICAL DO_FINDQCD_MUMU,DO_FINDQCD_MUJET,DO_FINDQCD_EJET
      LOGICAL DO_FINDWWG,DO_FIND_WWG_WE,DO_FIND_WWG_WMU
      LOGICAL DO_FINDTOP,DO_FINDTOP_EE,DO_FINDTOP_EMU
      LOGICAL DO_FINDTOP_EJET,DO_FINDTOP_MUJET
      LOGICAL DO_FINDTOP_MUMU,DO_NTUPLES,DO_MU_NTUPLE,DO_ELPH_NTUPLE
      LOGICAL DO_DPHI12_CUT,DO_MASS12_CUT,DO_MASS_DPHI_CUT
      LOGICAL DO_WAB_CUT,DO_ISOLCONE_SUB
      LOGICAL DO_RECO_STUDY,DO_TOPLEP_STUDY
      LOGICAL DO_VERTEX_CUTS,DO_TRIG_TEST,DO_TRIGFAIL_ONLY
      LOGICAL DO_RUN_RANGE_SELECT,DO_GOOD_RUN_SELECT
      LOGICAL DO_HISTOGRAMS,EL_DIAG_HISTOGRAMS,PH_DIAG_HISTOGRAMS
      LOGICAL MU_DIAG_HISTOGRAMS,DO_LIST_IDCUTS
      LOGICAL DO_EVENT_WRITE,WRITE_STA,WRITE_DST
      LOGICAL DO_MICRO_BLANK_CUT,DO_MRBS_LOSS_CUT
      LOGICAL LJ_DO_ISOL_CUT,LJ_DO_SHAPE_JET_DROP,LJ_DO_ISOLCONE_SUB
      LOGICAL DO_ISOL_CUT_WGAM,DO_DPHI_CUT,DO_MASS_CUT
      LOGICAL DO_ZSP_CORR,DO_UNDEVT_CORR,DO_OUTOFCONE_CORR
      LOGICAL DO_BIT_TEST,DO_FILTNAME_TEST
      LOGICAL DO_EVT_CHECK
C
      INTEGER IV,LUN,N,LEPTONS_LUNSUM,LEPTONS_LUNERR,JET_ALG
      INTEGER NOJT_MIN,MAX_NO_VERTEX_P,IER
      INTEGER NO_DIAG_EVT,COBREM_FLAG,WABREM_FLAG
      INTEGER LJ_NOMUON_MIN,LJ_NOMUON_MAX
      INTEGER LJ_NOMU_ISO_MIN,LJ_NOMU_ISO_MAX
      INTEGER LJ_NOMU_NONISO_MIN,LJ_NOMU_NONISO_MAX
      INTEGER LJ_NOJT_MIN,LJ_JET_MULT_MAX
      INTEGER WP_NOJT_MAX,JET_ISYS
      INTEGER NO_L1_SELECTED,L1_BITS_SELECTED(32)
      INTEGER NO_L2_SELECTED,L2_BITS_SELECTED(64)
      INTEGER NO_BAD_RUNS,NO_BAD_RUN_MAX,BAD_RUN_NOS(500)
      INTEGER FIRST_RUN,LAST_RUN
      INTEGER NO_FILTS,ILEN,I
C
      REAL VERSION_NO
      REAL ETAMU_MIN,ETAMU_MAX,PTMU_MIN,PTMU_MAX
      REAL MIN_VERTEX_Z,MAX_VERTEX_Z
      REAL ETAEL_MIN,ETAEL_MAX,PTEL_MIN,PTEL_MAX
      REAL ETAPH_MIN,ETAPH_MAX,PTPH_MIN,PTPH_MAX
      REAL ETAJT_MIN,ETAJT_MAX,PTJT_MIN,PTJT_MAX
      REAL MISSET_MINW_CORR,MISSET_MINW_CALO
      REAL MISSET_MIN_CORR,MISSET_MIN_CALO
      REAL DPHI_MAX_LEP12,PTMIN_JET1,PTMIN_JET2
      REAL ISOL_ELEC_PTMIN,ISOL_MUON_PTMIN,ISOL_PHOT_PTMIN
      REAL LJ_MUON1_PTMIN,LJ_MUON2_PTMIN
      REAL LJ_B2B_DPHIMIN
      REAL LJ_ELEC_PTMIN,LJ_PNUT2_ETMIN,LJ_PNUT3_ETMIN
      REAL LJ_JET1_PTMIN,LJ_JET2_PTMIN,LJ_JET3_PTMIN,LJ_JET4_PTMIN
      REAL LJ_JET5_PTMIN,LJ_JET6_PTMIN
      REAL LJ_PTMAX_NEXTJET,LJ_APLAN_MIN
      REAL LJ_JET_DROP_ETMIN,LJ_JET_DROP_ETMAX
      REAL LJ_JET_DROP_ETAMIN,LJ_JET_DROP_ETAMAX
      REAL LJ_HT_MIN,LJ_HT_JETPT_MIN,LJ_HT_JETETA_MAX
      REAL LJ_ISOL_MUON_PTMIN,LJ_NISOL_MUON_PTMIN
      REAL LJ_MIN_DR_MUJET,LJ_MAX_DR_MUJET
      REAL WP_PTMAX_JET,HV_COR,EM_COR(3)
      REAL WWG_PTMIN,WWG_ETMIN,WWG_PNUT2_MIN,WWG_PNUT3_MIN
      REAL WWG_MT_CLUS_MIN,WWG_DR_MIN
      REAL DR_MIN_LEP_JET,DR_ISOL_06_LEP,DR_MIN_PHO_JET
      REAL DPHI_MAX_LEP_PHOT,MIN_MASS_LEP_PHOT,MAX_MASS_LEP_PHOT
      REAL MASS12_MIN,MASS12_MAX
      REAL MT_MIN_WAB,MT_MAX_WAB,WAB_PNUT3_ETMIN,WAB_DRMAX_CLUS
      REAL MIN_DR_MUJET,MIN_DR_ELJET,MIN_DR_PHJET,MIN_DR_MUELPH
      REAL MUON_CORE_CONE,MUON_ISOL_CONE
      REAL MUON_ISOCONE_CF_CUT,MUON_ISOCONE_EF_CUT
C
      CHARACTER*32 FILT_LIST(128)
C
      DATA VERSION_NO,NO_BAD_RUN_MAX/ 6.01,500/
      IER = 0
C
C *** Get all latest parameter/Options Values
C
      CALL EZPICK('TOP_LEPTONS_RCP')
C
C *** Control Options - from TOP_LEPTONS_RCP -
C
      CALL EZGET_l('PRINT_CUTSUM',PRINT_LEPSUM,IER)
      IF (IER.EQ.0) CALL EZGET_l('LIST_IDCUTS',DO_LIST_IDCUTS,IER)
      IF (IER.EQ.0) CALL EZGET_i('PRINT_LUN',LEPTONS_LUNSUM,IER)
      IF (IER.EQ.0) CALL EZGET_i('ERROR_LUN',LEPTONS_LUNERR,IER)
      IF (IER.EQ.0) CALL EZGET_l('PLOT_HISTOGRAMS',DO_HISTOGRAMS,IER)
      IF(DO_HISTOGRAMS) THEN
        IF (IER.EQ.0) CALL EZGET_l('PLOT_ELECTRON_HIST',
     &    EL_DIAG_HISTOGRAMS,IER)
        IF (IER.EQ.0) CALL EZGET_l('PLOT_MUON_HIST',
     &    MU_DIAG_HISTOGRAMS,IER)
        IF (IER.EQ.0) CALL EZGET_l('PLOT_PHOTON_HIST',
     &    PH_DIAG_HISTOGRAMS,IER)
      ENDIF
      IF (IER.EQ.0) CALL EZGET_l('FILL_NTUPLES',DO_NTUPLES,IER)
      IF(DO_NTUPLES) THEN
        IF (IER.EQ.0) CALL EZGET_l('ELPH_NTUPLE',DO_ELPH_NTUPLE,IER)
        IF (IER.EQ.0) CALL EZGET_l('MU_NTUPLE',DO_MU_NTUPLE,IER)
      ENDIF
      IF (IER.EQ.0) CALL EZGET_l('WRITE_EVENTS_OUT',DO_EVENT_WRITE,IER)
      IF(DO_EVENT_WRITE) THEN
        IF(IER.EQ.0) CALL EZGET_l('WRITE_STA_FILE',WRITE_STA,IER)
        IF(IER.EQ.0) CALL EZGET_l('WRITE_DST_FILE',WRITE_DST,IER)
      ENDIF
      IF (IER.EQ.0) CALL EZGET_l('DO_DUMPS',DUMP_EVENT,IER)
      IF(DUMP_EVENT) THEN
        IF (IER.EQ.0) CALL EZGET_l('LONG_DUMPS',LONG_EVENT_DUMP,IER)
      ENDIF
      IF (IER.EQ.0) CALL EZGET_l('DO_DUPLICATE_CUT',DO_EVT_CHECK,IER)
C
C *** Generic Isolated Lepton cuts - used by most finders
C
      IF(IER.EQ.0) CALL EZGET('ISOLEL_PTMIN',ISOL_ELEC_PTMIN,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLMU_PTMIN',ISOL_MUON_PTMIN,IER)
      IF(IER.EQ.0) CALL EZGET('ISOLPH_PTMIN',ISOL_PHOT_PTMIN,IER)
      IF(IER.EQ.0) CALL EZGET_l('DO_ISOL_CONE_SUB',DO_ISOLCONE_SUB,
     1  IER)
      IF(IER.EQ.0) CALL EZGET('MU_JET_DRMIN',MIN_DR_MUJET,IER)
      IF(IER.EQ.0) CALL EZGET('EL_JET_DRMIN',MIN_DR_ELJET,IER)
      IF(IER.EQ.0) CALL EZGET('PH_JET_DRMIN',MIN_DR_PHJET,IER)
      IF(IER.EQ.0) CALL EZGET('CORECONE_SIZE',MUON_CORE_CONE,IER)
      IF(IER.EQ.0) CALL EZGET('ISOCONE_SIZE',MUON_ISOL_CONE,IER)
      IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_CF',MUON_ISOCONE_CF_CUT,
     1  IER)
      IF(IER.EQ.0) CALL EZGET('ISOCONE_CUT_EF',MUON_ISOCONE_EF_CUT,
     1  IER)
C
C *** Muon dE/dx corrections
C
        IF(IER.EQ.0) CALL EZGET_l('MU_CORR',CORR_MU,IER)
C
C *** W-finder specific cuts
C
      IF (IER.EQ.0) CALL EZGET_l('FINDW_CAND',DO_FINDW,IER)
      IF(DO_FINDW) THEN
        IF (IER.EQ.0) CALL EZGET_l('FINDW_ENU',DO_FIND_WE,IER)
        IF (IER.EQ.0) CALL EZGET_l('FINDW_MUNU',DO_FIND_WMU,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT2_ETMIN_W',MISSET_MINW_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT3_ETMIN_W',MISSET_MINW_CORR,IER)
      ENDIF
C
C *** Z-finder spacific cuts
C
      IF (IER.EQ.0) CALL EZGET_l('FINDZ_CAND',DO_FINDZ,IER)
      IF(DO_FINDZ) THEN
        IF (IER.EQ.0) CALL EZGET_l('FINDZ_EE',DO_FIND_ZEE,IER)
        IF (IER.EQ.0) CALL EZGET_l('FINDZ_MUMU',DO_FIND_ZMUMU,IER)
      ENDIF
C
C *** Wgamma-finder specific cuts
C
      IF (IER.EQ.0) CALL EZGET_l('FINDWWG_CAND',DO_FINDWWG,IER)
      IF(DO_FINDWWG) THEN
        IF (IER.EQ.0) CALL EZGET_l('FIND_WE_GAM',DO_FIND_WWG_WE,IER)
        IF (IER.EQ.0) CALL EZGET_l('FIND_WMU_GAM',DO_FIND_WWG_WMU,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_LEPTON_PTMIN',WWG_PTMIN,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_PHOTON_PTMIN',WWG_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_PNUT2_ETMIN',WWG_PNUT2_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_PNUT3_ETMIN',WWG_PNUT3_MIN,IER)
        IF (IER.EQ.0) 
     1     CALL EZGET('WWG_MIN_MT_CLUSTER',WWG_MT_CLUS_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_DRMIN_LEP_PHO',WWG_DR_MIN,IER)
        IF (IER.EQ.0) CALL EZGET_l('WWG_ISOL_CUT',DO_ISOL_CUT_WGAM,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_DRMIN_LEP_JET',DR_MIN_LEP_JET,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_MAX_ISO_SIG_LEP',DR_ISOL_06_LEP
     &                           ,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_DRMIN_PHO_JET',DR_MIN_PHO_JET,IER)
        IF (IER.EQ.0) CALL EZGET_l('WWG_DPHI_CUT',DO_DPHI_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_DPHI_LEP_PHO',DPHI_MAX_LEP_PHOT
     &                            ,IER)
        IF (IER.EQ.0) CALL EZGET_l('WWG_MASS_CUT',DO_MASS_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_MASS_MIN',MIN_MASS_LEP_PHOT,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_MASS_MAX',MAX_MASS_LEP_PHOT,IER)
        IF (IER.EQ.0) CALL EZGET_l('WWG_MASS_DPHI_CUT',DO_MASS_DPHI_CUT
     &                           ,IER)
      ENDIF
C
C *** top finders
C
      IF (IER.EQ.0) CALL EZGET_l('FINDTOP_CAND',DO_FINDTOP,IER)
      IF(DO_FINDTOP) THEN
        IF (IER.EQ.0) CALL EZGET_l('FINDTOP_EE',DO_FINDTOP_EE,IER)
        IF (IER.EQ.0) CALL EZGET_l('FINDTOP_EMU',DO_FINDTOP_EMU,IER)
        IF (IER.EQ.0) CALL EZGET_l('FINDTOP_MUMU',DO_FINDTOP_MUMU,IER)
C
C *** Top->dilepton finder specific cuts
C
        IF(DO_FINDTOP_EE.OR.DO_FINDTOP_EMU.OR.DO_FINDTOP_MUMU) THEN
C
          IF(IER.EQ.0) CALL EZGET_i('I_BREM_FLAG',COBREM_FLAG,IER)
          IF(IER.EQ.0) CALL EZGET('MU_ELPH_DRMIN',MIN_DR_MUELPH,IER)
          IF(IER.EQ.0) CALL EZGET_l('DO_MT_WAB_CUT',DO_WAB_CUT,IER)
          IF(IER.EQ.0) CALL EZGET_i('I_BREM_FLAG2',WABREM_FLAG,IER)
          IF(IER.EQ.0) CALL EZGET('WAB_PNUT3_ETMIN',WAB_PNUT3_ETMIN,IER)
          IF(IER.EQ.0) CALL EZGET('WAB_DR_MAX_MUCLUS',WAB_DRMAX_CLUS,
     1      IER)
          IF(IER.EQ.0) CALL EZGET('MT_MIN_WAB',MT_MIN_WAB,IER)
          IF(IER.EQ.0) CALL EZGET('MT_MAX_WAB',MT_MAX_WAB,IER)
C
          IF(IER.EQ.0) CALL EZGET('PNUT2_ETMIN',MISSET_MIN_CALO,IER)
          IF(IER.EQ.0) CALL EZGET('PNUT3_ETMIN',MISSET_MIN_CORR,IER)
C
          IF(IER.EQ.0) CALL EZGET_l('DO_DPHI12_CUT',DO_DPHI12_CUT,IER)
          IF(IER.EQ.0) CALL EZGET('MAX_DPHI_L12',DPHI_MAX_LEP12,IER)
          IF(IER.EQ.0) CALL EZGET_l('DO_MASS12_CUT',DO_MASS12_CUT,IER)
          IF(IER.EQ.0) CALL EZGET('MASS_L12_MIN',MASS12_MIN,IER)
          IF(IER.EQ.0) CALL EZGET('MASS_L12_MAX',MASS12_MAX,IER)
          IF(IER.EQ.0) CALL EZGET_l('DO_MASS_DPHI_CUT',DO_MASS_DPHI_CUT
     &         ,IER)
C
          IF(IER.EQ.0) CALL EZGET_i('JET_MULT_MIN',NOJT_MIN,IER)
          IF(IER.EQ.0) CALL EZGET('JET1_PTMIN',PTMIN_JET1,IER)
          IF(IER.EQ.0) CALL EZGET('JET2_PTMIN',PTMIN_JET2,IER)
        ENDIF
        IF (IER.EQ.0) CALL EZGET_l('FINDTOP_EJET',DO_FINDTOP_EJET,IER)
        IF (IER.EQ.0) CALL EZGET_l('FINDTOP_MUJET',DO_FINDTOP_MUJET,IER)
C
C *** Top->lepton+jets finder specific cuts
C
        IF(DO_FINDTOP_EJET.OR.DO_FINDTOP_MUJET) THEN
          IF(IER.EQ.0) CALL EZGET('LJ_MUON1_PTMIN',LJ_MUON1_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMUON_MIN',LJ_NOMUON_MIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMUON_MAX',LJ_NOMUON_MAX,IER)
          IF(IER.EQ.0) CALL EZGET_l('LJ_DO_ISOL_CUT',LJ_DO_ISOL_CUT,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMU_ISO_MIN',LJ_NOMU_ISO_MIN
     &         ,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMU_ISO_MAX',LJ_NOMU_ISO_MAX
     &         ,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_MUON2_PTMIN',LJ_MUON2_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMU_NONISO_MIN',
     &                            LJ_NOMU_NONISO_MIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_NOMU_NONISO_MAX',
     &                            LJ_NOMU_NONISO_MAX,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_B2B_DPHIMIN',LJ_B2B_DPHIMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_ELEC_PTMIN',LJ_ELEC_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_PNUT2_ETMIN',LJ_PNUT2_ETMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_PNUT3_ETMIN',LJ_PNUT3_ETMIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_JET_MULT_MIN',LJ_NOJT_MIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET1_PTMIN',LJ_JET1_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET2_PTMIN',LJ_JET2_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET3_PTMIN',LJ_JET3_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET4_PTMIN',LJ_JET4_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET5_PTMIN',LJ_JET5_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET6_PTMIN',LJ_JET6_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET_i('LJ_JET_MULT_MAX',LJ_JET_MULT_MAX
     &         ,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_PTMAX_NEXTJET',LJ_PTMAX_NEXTJET,
     &                            IER)
          IF(IER.EQ.0) CALL EZGET('LJ_APLAN_MIN',LJ_APLAN_MIN,IER)
          IF(IER.EQ.0) CALL EZGET_l('LJ_DO_SHAPE_JET_DROP',
     &                             LJ_DO_SHAPE_JET_DROP,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET_DROP_ETMIN',
     &                             LJ_JET_DROP_ETMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET_DROP_ETMAX',
     &                             LJ_JET_DROP_ETMAX,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET_DROP_ETAMIN',
     &                             LJ_JET_DROP_ETAMIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_JET_DROP_ETAMAX',
     &                             LJ_JET_DROP_ETAMAX,IER)


          IF(IER.EQ.0) CALL EZGET('LJ_HT_MIN',LJ_HT_MIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',LJ_HT_JETPT_MIN,IER)
          IF(IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',LJ_HT_JETETA_MAX,
     1      IER)
          IF(IER.EQ.0) CALL EZGET('ISOLMU_PTMIN',LJ_ISOL_MUON_PTMIN,IER)
          IF(IER.EQ.0) CALL EZGET('NISOLMU_PTMIN',LJ_NISOL_MUON_PTMIN,
     &      IER)
          IF(IER.EQ.0) CALL EZGET('MU_JET_DRMIN',LJ_MIN_DR_MUJET,IER)
          IF(IER.EQ.0) CALL EZGET('MU_JET_DRMAX',LJ_MAX_DR_MUJET,IER)
          IF(IER.EQ.0) CALL EZGET_l('DO_ISOL_CONE_SUB'
     &         ,LJ_DO_ISOLCONE_SUB,IER)
        ENDIF
      ENDIF
C
C *** QCD finder specific cuts
C
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_CAND',DO_FINDQCD,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_EE',DO_FINDQCD_EE,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_EMU',DO_FINDQCD_EMU,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_MUMU',DO_FINDQCD_MUMU,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_MUJET',DO_FINDQCD_MUJET,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDQCD_EJET',DO_FINDQCD_EJET,IER)
C
C *** Wpair finder specific cuts
C
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_CAND',DO_FINDWPAIR,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_EE',DO_FINDWPAIR_EE,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_EMU',DO_FINDWPAIR_EMU,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_MUMU',DO_FINDWPAIR_MUMU,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_MUJET',DO_FINDWPAIR_MUJET
     &     ,IER)
      IF(IER.EQ.0) CALL EZGET_l('FINDWPAIR_EJET',DO_FINDWPAIR_EJET,IER)
C
      IF(DO_FINDWPAIR) THEN
        IF(IER.EQ.0) CALL EZGET('WP_JET_PTMAX',WP_PTMAX_JET,IER)
        IF(IER.EQ.0) CALL EZGET_i('WP_JET_MULT_MAX',WP_NOJT_MAX,IER)
      ENDIF
C
C
C *** Cuts
C ***      Global Event/Run quantities
C
      IF(IER.EQ.0) CALL EZGET_l('DO_VERTEX_CUTS',DO_VERTEX_CUTS,IER)
      IF(DO_VERTEX_CUTS) THEN
        IF(IER.EQ.0) CALL EZGET_i('VERTEX_MAX_NO_P',MAX_NO_VERTEX_P,IER)
          IF(IER.EQ.0) CALL EZGET('VERTEX_ZMIN',MIN_VERTEX_Z,IER)
          IF(IER.EQ.0) CALL EZGET('VERTEX_ZMAX',MAX_VERTEX_Z,IER)
      ENDIF
C
C ***      Level 1/2 Trigger selection
C
      IF(IER.EQ.0) CALL EZGET_l('DO_TRIGGER_SELECT',DO_TRIG_TEST,IER)
      IF(DO_TRIG_TEST) THEN
        IF(IER.EQ.0)
     1    CALL EZGET_l('DO_TRIGFAIL_SELECT',DO_TRIGFAIL_ONLY,IER)
C###
        IF(IER.EQ.0) CALL EZGET_l('DO_BIT_SELECT',DO_BIT_TEST,IER)
        IF(DO_BIT_TEST)THEN
C###
          IF(IER.EQ.0) CALL EZGETA_i('L1_BITS',0,0,0,NO_L1_SELECTED,IER)
          IF(IER.EQ.0) CALL EZGET_iarr('L1_BITS',L1_BITS_SELECTED,IER)
          IF(IER.EQ.0)
     1       CALL EZGETA_i('L2_FILTERS',0,0,0,NO_L2_SELECTED,IER)
          IF(IER.EQ.0) CALL EZGET_iarr('L2_FILTERS',L2_BITS_SELECTED
     &         ,IER)
C####
        ENDIF
        IF(IER.EQ.0) CALL EZGET_l('DO_FILTERNAME_SELECT',
     1                          DO_FILTNAME_TEST,IER)
        IF(DO_FILTNAME_TEST)THEN
          IF(IER.EQ.0) CALL EZGET_NUMBER_STRINGS('FILTER_LIST',
     1                                          NO_FILTS,IER)
          DO I = 1,NO_FILTS
            IF(IER.EQ.0) CALL EZGETS('FILTER_LIST',I,
     1                               FILT_LIST(I),ILEN,IER)
          END DO
        ENDIF
C####
      ENDIF
      IF(IER.EQ.0) CALL EZGET_l('DO_MICRO_BLANK_CUT',
     1  DO_MICRO_BLANK_CUT,IER)
      IF(IER.EQ.0) CALL EZGET_l('DO_MRBS_LOSS_CUT',
     1  DO_MRBS_LOSS_CUT,IER)
C
C ***      Bad Run Test Information
C
      CALL EZGET_l('DO_RUN_RANGE_SELECT',DO_RUN_RANGE_SELECT,IER)
      IF(DO_RUN_RANGE_SELECT) THEN
        IF(IER.EQ.0) CALL EZGET_i('FIRST_RUN',FIRST_RUN,IER)
        IF(IER.EQ.0) CALL EZGET_i('LAST_RUN',LAST_RUN,IER)
      ENDIF
      IF(IER.EQ.0) CALL EZGET_l('DO_GOOD_RUN_SELECT',
     1  DO_GOOD_RUN_SELECT,IER)
C
C *** Jet Algorithm 
C
      IF (IER.EQ.0) CALL EZGET_i('JETS_ALGORITHM',JET_ALG,IER)
C
C *** Jet Energy Corrections
C
      IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
      IF (IER.EQ.0) CALL EZGET_i('JETS_ISYS',JET_ISYS,IER)
      IF (IER.EQ.0) CALL EZGET_l('DO_ZSP_CORR',DO_ZSP_CORR,IER)
      IF (IER.EQ.0) CALL EZGET_l('DO_UNDEVT_CORR',DO_UNDEVT_CORR,IER)
      IF (IER.EQ.0)
     1  CALL EZGET_l('DO_OUTOFCONE_CORR',DO_OUTOFCONE_CORR,IER)
C
C *** em energy scale corrections
C
      IF (IER.EQ.0) CALL EZGET_l('EM_CORR',CORR_EM,IER)
      IF (IER.EQ.0) CALL EZGET('HV_SCALE_FACTOR',HV_COR,IER)
      IF (IER.EQ.0) CALL EZGETA_i('EM_SCALE_FACTORS',0,0,0,N,IER)
      IF (IER.EQ.0) CALL EZGETA('EM_SCALE_FACTORS',1,N,1,EM_COR,IER)
C
C *** ZEBRA Bank cuts on Leptons, Photons and Jets
C *** Muons --- PMUO
C
      IF (IER.EQ.0) CALL EZGET('PMUO_ETAMIN',ETAMU_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PMUO_ETAMAX',ETAMU_MAX,IER)
      IF (IER.EQ.0) CALL EZGET('PMUO_PTMIN',PTMU_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PMUO_PTMAX',PTMU_MAX,IER)
C
C *** Electrons --- PELC
C
      IF (IER.EQ.0) CALL EZGET('PELC_ETAMIN',ETAEL_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PELC_ETAMAX',ETAEL_MAX,IER)
      IF (IER.EQ.0) CALL EZGET('PELC_PTMIN',PTEL_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PELC_PTMAX',PTEL_MAX,IER)
C
C *** Phtons --- PPHO
C
      IF (IER.EQ.0) CALL EZGET('PPHO_ETAMIN',ETAPH_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PPHO_ETAMAX',ETAPH_MAX,IER)
      IF (IER.EQ.0) CALL EZGET('PPHO_PTMIN',PTPH_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('PPHO_PTMAX',PTPH_MAX,IER)
C
C *** Jets --- JETS
C
      IF (IER.EQ.0) CALL EZGET('JETS_ETAMIN',ETAJT_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETAJT_MAX,IER)
      IF (IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJT_MIN,IER)
      IF (IER.EQ.0) CALL EZGET('JETS_PTMAX',PTJT_MAX,IER)
C
C *** Diagnostic analysis
C ***    .... for RECO testing
C
      IF (IER.EQ.0) CALL EZGET_l('DO_RECO_DIAGNOSTICS',DO_RECO_STUDY
     &     ,IER)
C
C ***    .... for TOP_LEPTONS
C
      IF (IER.EQ.0) CALL EZGET_l('DO_DIAGNOSTIC_DUMPS',DO_TOPLEP_STUDY,
     &  IER)
      IF(DO_TOPLEP_STUDY) THEN
        IF (IER.EQ.0) CALL EZGET_i('NO_EVT_DIAGNOSTICS',NO_DIAG_EVT,IER)
      ENDIF
      CALL EZRSET
      IF (IER.NE.0) CALL ERRMSG('Error in TOP_LEPTONS_RCP',
     &    'TOP_LEPTONS_INIT_SUMMRY',' ','F')
C
C *** Bad Run List - from BAD_RUN_RCP -
C
      IF(DO_GOOD_RUN_SELECT) THEN
        CALL INRCP('BAD_RUN_RCP',IER)
        IF(IER.NE.0)
     1    CALL ERRMSG('BAD_RUN_RCP not found',
     2      'TOP_LEPTONS_INIT_SUMMRY',' ','W')
        CALL EZPICK('BAD_RUN_RCP')
        IF(IER.EQ.0) CALL EZGETA_i('BAD_RUN_NOS',0,0,0,NO_BAD_RUNS,IER)
        IF(NO_BAD_RUNS.LE.NO_BAD_RUN_MAX) THEN
          IF(IER.EQ.0) CALL EZGET_iarr('BAD_RUN_NOS',BAD_RUN_NOS,IER)
        ELSE
          CALL ERRMSG('Too Many Bad Runs To Store',
     1      'TOP_LEPTONS_INIT_SUMMRY',' ','F')
        ENDIF
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error in BAD_RUN_RCP',
     &    'TOP_LEPTONS_INIT_SUMMRY',' ','F')
      ENDIF
C
C -----------------------------------------------------------------
C ****  Now write the summary of cuts to output file
C
      LUN=LEPTONS_LUNSUM
C
      IF(PRINT_LEPSUM)THEN
        WRITE(LUN,1000) VERSION_NO
        If(DO_TOPLEP_STUDY) GO TO 100
C
C *** Top-finder
C
        IF(DO_FINDTOP) THEN
          WRITE(LUN,1080) DO_FINDTOP_EMU,DO_FINDTOP_EE,DO_FINDTOP_MUMU,
     1      DO_FINDTOP_EJET,DO_FINDTOP_MUJET
        ELSE
          WRITE(LUN,1081)
        ENDIF
C
C *** W-finder
C
        IF(DO_FINDW) THEN
          WRITE(LUN,1030) DO_FIND_WE,DO_FIND_WMU 
        ELSE
          WRITE(LUN,1031)
        ENDIF
C
C *** Z-finder
C
        IF(DO_FINDZ) THEN
          WRITE(LUN,1090) DO_FIND_ZEE,DO_FIND_ZMUMU 
        ELSE
          WRITE(LUN,1091)
        ENDIF
C
C *** WWgamma-finder
C
        IF(DO_FINDWWG) THEN
          WRITE(LUN,1092) DO_FIND_WWG_WE,DO_FIND_WWG_WMU
        ELSE
          WRITE(LUN,1093) 
        ENDIF
C
C *** QCD background finder
C
        IF(DO_FINDQCD) THEN
          WRITE(LUN,1094) DO_FINDQCD_EE,DO_FINDQCD_EMU,DO_FINDQCD_MUMU
     1      ,DO_FINDQCD_MUJET,DO_FINDQCD_EJET
        ELSE
          WRITE(LUN,1095)
        ENDIF
C
C *** Wpair background finder
C
        IF(DO_FINDWPAIR) THEN
          WRITE(LUN,1096) DO_FINDWPAIR_EMU,DO_FINDWPAIR_EE,
     1      DO_FINDWPAIR_MUMU,DO_FINDWPAIR_MUJET,DO_FINDWPAIR_EJET
        ELSE
          WRITE(LUN,1097)
        ENDIF
C
C *** Jet algorithm + Et threshold, Missing Et factor
C
        WRITE(LUN,1010) JET_ALG
C
C *** Jet Corrections
C
        IF(CORR_JETS) THEN
          WRITE(LUN,1020) JET_ISYS,DO_ZSP_CORR,DO_UNDEVT_CORR,
     1      DO_OUTOFCONE_CORR
        ELSE
          WRITE(LUN,1021)
        ENDIF
C
C *** em scale Corrections
C
        IF(CORR_EM) THEN
          WRITE(LUN,1022) HV_COR,EM_COR 
        ELSE
          WRITE(LUN,1023)
        ENDIF
C
C *** isolated muon dE/dx correction
C
        IF(CORR_MU) THEN
          WRITE(LUN,1024)
        ELSE
          WRITE(LUN,1025)
        ENDIF
C
C *** Histogramming
C
        IF(DO_HISTOGRAMS) THEN
          WRITE(LUN,1040) MU_DIAG_HISTOGRAMS,
     1      EL_DIAG_HISTOGRAMS,PH_DIAG_HISTOGRAMS
        ELSE
          WRITE(LUN,1041)
        ENDIF
C
C *** Ntuple handling
C
        IF(DO_NTUPLES) THEN
          WRITE(LUN,1050) DO_MU_NTUPLE,DO_ELPH_NTUPLE
        ELSE
          WRITE(LUN,1051)
        ENDIF
 100    CONTINUE
C
C *** Event dumps
C
        IF(DUMP_EVENT) THEN
          WRITE(LUN,1060) LONG_EVENT_DUMP
        ELSE
          WRITE(LUN,1061)
        ENDIF
        If(DO_TOPLEP_STUDY) GO TO 200

C *** Write-out of selected events to new file
C
        IF(DO_EVENT_WRITE) THEN
          WRITE(LUN,1070) WRITE_STA,WRITE_DST
        ELSE
          WRITE(LUN,1071)
        ENDIF
  200   CONTINUE
C
C *** Info for diagnostic running
C
        IF(DO_TOPLEP_STUDY) THEN
          WRITE(LUN,1075) NO_DIAG_EVT
        ENDIF
C
        IF(DO_RECO_STUDY) THEN
          WRITE(LUN,1076)
        ENDIF
C
C *** when running in diagnostic dump mode disable full summary
C *** printing
C
        IF(DO_TOPLEP_STUDY) THEN
          GO TO 999
        ENDIF
C
        IF(DO_LIST_IDCUTS) THEN
          WRITE(LUN,2000)
C
C -----------------------------------------------------------------
C *** List Particle id/validation cuts
C *** a.) Muons
C
          CALL TOP_LEPTONS_CLEANMU_INIT_SUMMRY(LUN)
C
C *** b.) Electron and Photon id
C
          CALL TOP_LEPTONS_CLEANEM_INIT_SUMMRY(LUN)
        ENDIF
C
C -----------------------------------------------------------------
C *** Next list 'Global Event' and 'Physics' cuts on jet and lepton
C *** selection : 
C
        WRITE(LUN,3000)
C
C *** Duplicate Event Skip
C
        IF(DO_EVT_CHECK) THEN
          WRITE(LUN,3005)
        ENDIF
C
C *** Run Range Checking
C
        IF(DO_RUN_RANGE_SELECT) THEN
          WRITE(LUN,3010) FIRST_RUN,LAST_RUN
        ELSE
          WRITE(LUN,3011)
        ENDIF
C
C *** Bad Run Checking
C
        IF(DO_GOOD_RUN_SELECT) THEN
          WRITE(LUN,3012) NO_BAD_RUNS
          IF(NO_BAD_RUNS.GE.1) WRITE(LUN,3013)
     1      (BAD_RUN_NOS(IV),IV=1,NO_BAD_RUNS)
        ELSE
          WRITE(LUN,3014)
        ENDIF
C
C *** Level 1 and 2 Trigger selection
C
        IF(DO_TRIG_TEST) THEN
          WRITE(LUN,3020)
          IF(DO_TRIGFAIL_ONLY) WRITE(LUN,3021)
C##
          IF(DO_BIT_TEST)THEN
C##
          IF(NO_L1_SELECTED.GE.1) WRITE(LUN,3022) 
     1      (L1_BITS_SELECTED(IV),IV=1,NO_L1_SELECTED)
          IF(NO_L2_SELECTED.GE.1) WRITE(LUN,3023)
     1      (L2_BITS_SELECTED(IV),IV=1,NO_L2_SELECTED)
C###
          ENDIF
          IF(DO_FILTNAME_TEST)THEN
            IF(NO_FILTS.GE.1) THEN
              WRITE(LUN,3038)
              WRITE(LUN,3039)(FILT_LIST(IV),IV=1,NO_FILTS)
            ENDIF
          ENDIF
C###
        ELSE
          WRITE(LUN,3025)
        ENDIF
        IF(DO_MICRO_BLANK_CUT) THEN
          WRITE(LUN,3024)
        ELSE
          WRITE(LUN,3026)
        ENDIF
C
        IF(DO_MRBS_LOSS_CUT) THEN
          WRITE(LUN,3027)
        ELSE
          WRITE(LUN,3028)
        ENDIF
C
C *** Primary Vertex Selection Cuts
C
        IF(DO_VERTEX_CUTS) THEN
          WRITE(LUN,3030) MAX_NO_VERTEX_P,MIN_VERTEX_Z,MAX_VERTEX_Z
        ELSE
          WRITE(LUN,3031)
        ENDIF
C
C *** a.) Muons
C
        WRITE(LUN,3050) ETAMU_MIN,ETAMU_MAX,PTMU_MIN,PTMU_MAX
C
C *** b.) Electons
C
        WRITE(LUN,3200) ETAEL_MIN,ETAEL_MAX,PTEL_MIN,PTEL_MAX
C
C *** c.) Photons
C
        WRITE(LUN,3400) ETAPH_MIN,ETAPH_MAX,PTPH_MIN,PTPH_MAX
C
C *** d.) Jets
C
        WRITE(LUN,3600) ETAJT_MIN,ETAJT_MAX,PTJT_MIN,PTJT_MAX
        IF(DO_FINDWPAIR) THEN
          WRITE(LUN,3610) WP_NOJT_MAX,WP_PTMAX_JET
        ENDIF
C
C -----------------------------------------------------------------
C ***
C *** Top selection cuts
C       
        IF(DO_FINDTOP) THEN
          WRITE(LUN,4000)
C
C *** Dilepton finder
C
          IF(DO_FINDTOP_EE.OR.DO_FINDTOP_EMU.OR.DO_FINDTOP_MUMU) THEN
            IF(DO_DPHI12_CUT) THEN
              WRITE(LUN,4500)
            ELSE
              WRITE(LUN,4501)
            ENDIF
            IF(DO_MASS12_CUT) THEN
              WRITE(LUN,4510)
            ELSE
              WRITE(LUN,4511)
            ENDIF
            IF(DO_MASS_DPHI_CUT) THEN
              WRITE(LUN,4520)
            ELSE
              WRITE(LUN,4521)
            ENDIF
            IF(DO_WAB_CUT) THEN
              WRITE(LUN,4530)              
            ELSE
              WRITE(LUN,4531)              
            ENDIF
            IF(DO_ISOLCONE_SUB) THEN
              WRITE(LUN,4540)
            ELSE
              WRITE(LUN,4541)
            ENDIF
            WRITE(LUN,4010)  ISOL_ELEC_PTMIN,ISOL_MUON_PTMIN,
     1        ISOL_PHOT_PTMIN
            WRITE(LUN,4011) MUON_CORE_CONE,MUON_ISOL_CONE,
     1        MUON_ISOCONE_CF_CUT,MUON_ISOCONE_EF_CUT,
     2        MIN_DR_MUJET
            WRITE(LUN,4012) MIN_DR_ELJET,MIN_DR_PHJET
            WRITE(LUN,4013) COBREM_FLAG,MIN_DR_MUELPH,
     1        WABREM_FLAG,WAB_PNUT3_ETMIN,WAB_DRMAX_CLUS,
     2        MT_MIN_WAB,MT_MAX_WAB
            WRITE(LUN,4014) DPHI_MAX_LEP12,MASS12_MIN,MASS12_MAX
            WRITE(LUN,4015) MISSET_MIN_CALO,MISSET_MIN_CORR
            WRITE(LUN,4016) NOJT_MIN,PTMIN_JET1,PTMIN_JET2 
          ENDIF
C
C *** Lepton_jets finder
C
          IF(DO_FINDTOP_EJET.OR.DO_FINDTOP_MUJET) THEN
C
C *** Check that HT cuts are consistent with jet pre-selection
C *** - if not then reset
C
            IF(LJ_HT_JETPT_MIN.LT.PTJT_MIN) LJ_HT_JETPT_MIN=PTJT_MIN
            IF(LJ_HT_JETETA_MAX.GT.ETAJT_MAX)
     1        LJ_HT_JETETA_MAX=ETAJT_MAX
            WRITE(LUN,4020)LJ_MUON1_PTMIN,LJ_NOMUON_MIN,LJ_NOMUON_MAX,
     1        LJ_B2B_DPHIMIN,LJ_ELEC_PTMIN,
     2        LJ_PNUT2_ETMIN,LJ_PNUT3_ETMIN,
     3        LJ_NOJT_MIN,LJ_JET1_PTMIN,LJ_JET2_PTMIN,LJ_JET3_PTMIN,
     4        LJ_JET4_PTMIN,LJ_JET5_PTMIN,LJ_JET6_PTMIN,
     5        LJ_JET_MULT_MAX,LJ_PTMAX_NEXTJET
            IF(.NOT.LJ_DO_ISOL_CUT) THEN
              WRITE(LUN,4040)
            ELSE
              WRITE(LUN,4041)
              IF(LJ_ISOL_MUON_PTMIN.GT.LJ_MUON1_PTMIN) 
     1          LJ_MUON1_PTMIN = LJ_ISOL_MUON_PTMIN
              IF (LJ_NISOL_MUON_PTMIN.GT.LJ_MUON2_PTMIN)
     1          LJ_MUON2_PTMIN = LJ_NISOL_MUON_PTMIN
              WRITE(LUN,4042)LJ_MUON1_PTMIN,LJ_NOMU_ISO_MIN,
     1          LJ_NOMU_ISO_MAX,LJ_MUON2_PTMIN,LJ_NOMU_NONISO_MIN,
     2          LJ_NOMU_NONISO_MAX,
     3          LJ_MIN_DR_MUJET,LJ_MAX_DR_MUJET
              IF(.NOT.LJ_DO_ISOLCONE_SUB) THEN
                WRITE(LUN,4043)
              ELSE
                WRITE(LUN,4044)
              ENDIF
            ENDIF
            WRITE(LUN,4050) LJ_HT_MIN,LJ_HT_JETPT_MIN,LJ_HT_JETETA_MAX
            WRITE(LUN,4051)LJ_APLAN_MIN
            IF (LJ_DO_SHAPE_JET_DROP)THEN
              WRITE(LUN,4052)LJ_JET_DROP_ETMIN,LJ_JET_DROP_ETMAX,
     1          LJ_JET_DROP_ETAMIN,LJ_JET_DROP_ETAMAX
            ENDIF
          ENDIF
        ENDIF
C
C -----------------------------------------------------------------
C *** W selection cuts
C
        IF(DO_FINDW) THEN
          WRITE(LUN,5000) ISOL_ELEC_PTMIN,ISOL_MUON_PTMIN,
     1      ISOL_PHOT_PTMIN,MISSET_MINW_CALO,MISSET_MINW_CORR
        ENDIF
C
C -----------------------------------------------------------------
C *** Z selection cuts
C
        IF(DO_FINDZ) THEN
          WRITE(LUN,5030) ISOL_MUON_PTMIN,ISOL_MUON_PTMIN
        ENDIF
C
C -----------------------------------------------------------------
C *** WWgamma selection cuts
C
        IF(DO_FINDWWG) THEN
          WRITE(LUN,5040) WWG_PTMIN,WWG_ETMIN,WWG_PNUT2_MIN,
     1      WWG_PNUT3_MIN,WWG_MT_CLUS_MIN,WWG_DR_MIN
          IF(DO_ISOL_CUT_WGAM)THEN
           WRITE(LUN,5050)DR_MIN_LEP_JET,DR_ISOL_06_LEP,DR_MIN_PHO_JET
          ENDIF
          IF(DO_DPHI_CUT)THEN
           WRITE(LUN,5060)DPHI_MAX_LEP_PHOT
          ENDIF
          IF(DO_MASS_CUT)THEN
           WRITE(LUN,5070)MIN_MASS_LEP_PHOT,MAX_MASS_LEP_PHOT
          ENDIF
          IF(DO_MASS_DPHI_CUT)THEN
           WRITE(LUN,4520)
          ENDIF
        ENDIF
C------------------------------------------------------------------
        WRITE(LUN,6000)
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
 1000 FORMAT(1H1,9X,'TOP_LEPTONS Package(V',F4.2,') Initialisation',
     1  /10X,40(1H-),//)
 1010 FORMAT(10X,'Job Control Options : ',/,10X,19(1H-)//,
     1  5X,' Jet Algorithm (1-4 : Cone_0.7/0.5/0.3/NN)   = ',I8)
 1020 FORMAT(5X,' Jet Corrections Used :',/,
     1 15X,' Correction Flag   (0/1/2 : nominal/low/high) = ',I3,/,
     2 15X,' Zero Suppression             (-1/0 : on/off) = ',I3,/,
     3 15X,' Underlying Event Subtraction (-1/0 : on/off) = ',I3,/,
     4 15X,' Out of Cone Correction       (-1/0 : on/off) = ',I3)
 1021 FORMAT(5X,' Jet Corrections Not Applied ')
 1022 FORMAT(5X,' em Energy Scale Corrections Used : ',/,
     1 15X,' High Voltage Correction (RECO 10 only) = ',F6.3,/,
     2 15X,' em Scale Corrections      (ECN,CC,ECS) = ',3F6.3)
 1023 FORMAT(5X,' em Energy Scale Corrections not applied ')
 1024 FORMAT(5X,' Measured dE/dx used for isolated muons ')
 1025 FORMAT(5X,' Muon dE/dx corrections not applied ') 
 1030 FORMAT(5X,' W Finder Enabled :',/,
     1  5X,'      W -> enu finder      (-1/0 : on/off) = ',I8,/,
     2  5X,'      W -> munu finder     (-1/0 : on/off) = ',I8,/)
 1031 FORMAT(5X,' W Finder Disabled ',/)
 1040 FORMAT(5X,' Monitoring Histogramming Enabled  : ',/,
     1  5X,'      muon diagnostics       (-1/0 : on/off) = ',I8,/,
     2  5X,'      electron diagnostics   (-1/0 : on/off) = ',I8,/,
     3  5X,'      photon diagnostics     (-1/0 : on/off) = ',I8)
 1041 FORMAT(5X,' Monitoring Histogramming Disabled ')
 1050 FORMAT(5X,' Physics Ntuple Enabled :',/,
     1  5X,'      muon diagnostics       (-1/0 : on/off) = ',I8,/,
     2  5X,'      electron/photn diag.   (-1/0 : on/off) = ',I8)
 1051 FORMAT(5X,' Ntuple Filling Disabled ')
 1060 FORMAT(5X,' Event Dumps Enabled : ',/,
     1  5X,'      detailed dumps         (-1/0 : on/off) = ',I8)
 1061 FORMAT(5X,' Event Dumps Diabled ')
 1070 FORMAT(5X,' Write-out of Selected Events Enabled :',/,
     1  5X,'      STA output file        (-1/0 : on/off) = ',I8,/,
     2  5X,'      DST output file        (-1/0 : on/off) = ',I8)
 1071 FORMAT(5X,' Write-out of Selected Events Disabled ')
 1075 FORMAT(5X,' Top_Leptons Running in Daignostic Mode :',/,
     1 5X,'       No raw input events to be dumped = ',I5)
 1076 FORMAT(5X,' Reco study output enabled : ',/,
     1 5X,'       Muon diagnostics witten to LUN 16 ')
 1080 FORMAT(5X,' Top Finder Enabled :',/,
     1  5X,'      ttbar-> emu finder     (-1/0 : on/off) = ',I8,/,
     2  5X,'      ttbar-> ee finder      (-1/0 : on/off) = ',I8,/,
     3  5X,'      ttbar-> mumu finder    (-1/0 : on/off) = ',I8,/,
     4  5X,'      ttbar-> e+jets finder  (-1/0 : on/off) = ',I8,/,
     5  5X,'      ttbar-> mu+jets finder (-1/0 : on/off) = ',I8,/)
 1081 FORMAT(5X,' Top Finder Disabled ',/)
 1090 FORMAT(5X,' Z Finder Enabled :',/,
     1  5X,'      Z -> ee finder       (-1/0 : on/off) = ',I8,/,
     2  5X,'      Z -> mumu finder     (-1/0 : on/off) = ',I8,/)
 1091 FORMAT(5X,' Z Finder Disabled ',/)
 1092 FORMAT(5X,' WWgamma Finder Enabled :',/,
     1  5X,'      W->enu+gamma finder  (-1/0 : on/off) = ',I8,/,
     2  5X,'      W->munu_gamma finder (-1/0 : on/off) = ',I8,/)
 1093 FORMAT(5X,' WWgamma Finder Disabled ',/)
 1094 FORMAT(5X,' QCD Background Finder Enabled :',/,
     1  5X,'      QCD -> emu finder      (-1/0 : on/off) = ',I8,/,
     2  5X,'      QCD -> ee finder       (-1/0 : on/off) = ',I8,/,
     3  5X,'      QCD -> mumu finder     (-1/0 : on/off) = ',I8,/,
     4  5X,'      QCD -> e+jets finder   (-1/0 : on/off) = ',I8,/,
     5  5X,'      QCD -> mu+jets finder  (-1/0 : on/off) = ',I8,/)
 1095 FORMAT(5X,' QCD Background Finder Disabled ',/)
 1096 FORMAT(5X,' Wpair Background Finder Enabled :',/,
     1  5X,'      Wpair -> emu finder    (-1/0 : on/off) = ',I8,/,
     2  5X,'      Wpair -> ee finder     (-1/0 : on/off) = ',I8,/,
     3  5X,'      Wpair -> mumu finder   (-1/0 : on/off) = ',I8,/,
     4  5X,'      Wpair -> e+jets finder (-1/0 : on/off) = ',I8,/,
     5  5X,'      Wpair -> mu+jets finder(-1/0 : on/off) = ',I8,/)
 1097 FORMAT(5X,' Wpair Background Finder Disabled ',/)
C
 2000 FORMAT(/,10X,'Particle id/validation Cuts : ',/10X,29(1H-)/)
C
 3000 FORMAT(//10X,'Event Selection Cuts : ',/10X,22(1H-)/)
 3005 FORMAT(10X,'a,) Event Validity Checks :',//,
     1 5X,' Duplicate Events Rejected (Data Only)')
 3007 FORMAT(10X,'a,) Event Validity Checks :',//,
     1 5X,' No Duplicate Event Rejection  (Data Only)')
 3010 FORMAT(10X,'b.) Run Validity Checks : ',//,
     1 5X,' First / Last Runs Requested = ',2I10)
 3011 FORMAT(10X,'b.) Run Validity Checks : ',//,
     1 5X,' No checks on Run Range Done ')
 3012 FORMAT(5X,' No.of Bad Runs = ',I5)
 3013 FORMAT(5X,' Run List : ',20(/,5X,10I7))
 3014 FORMAT(5X,' No bad run checking done ')
 3020 FORMAT(/,10X,'c.) Level 1 / Level 2 Trigger Selection ',/)
 3021 FORMAT(5X,' Keep only events which FAIL trigger selection ')
 3022 FORMAT(5X,' Level 1 Trigger Bits Requested = ',32(I3)) 
 3023 FORMAT(5X,' Level 2 Filter Bits Requested  = ',32(I3),
     1  /33X,32(I3))
 3024 FORMAT(5X,' Micro Blank Veto Applied ')
 3025 FORMAT(10X,'c.) Level 1 / Level 2 Trigger Selection ',/,
     1  5X,' No cuts applied => all triggers accepted ')
 3026 FORMAT(5X,' Micro Blank Veto Not Applied ')     
 3027 FORMAT(5X,' MRBS Loss Veto Applied ')
 3028 FORMAT(5X,' MRBS Loss Veto Not Applied ')     
 3030 FORMAT(/,10X,'d.) Primary Vertex Selection Cuts ',//,
     1  5X,' Max allowed multiplicity            = ',I8,/,
     2  5X,' Vertex z-position     : min,max     = ',2F8.1)
 3031 FORMAT(/,10X,'d.) Primary Vertex Selection Cuts ',//,
     1  5X,' No cuts applied => all events accepted ')
 3038 FORMAT(5X,' Filter Names Requested = ')
 3039 FORMAT(10X,A32,5X,A32,/)
 3050 FORMAT(/,10X,'e.) Lepton,Photon and Jet BANK selection',//,
     2  5X,' Muons (PMUO)          : eta min,max = ',2F8.1,/,
     3  5X,'                         pt  min,max = ',2F8.1)
 3200 FORMAT(
     1  5X,' Elecrons (PELC)       : eta min,max = ',2F8.1,/,
     2  5X,'                         pt  min,max = ',2F8.1)
 3400 FORMAT(5X,' Photons (PPHO)        : eta min,max = ',2F8.1,/,
     1  5X,'                         pt  min,max = ',2F8.1)
 3600 FORMAT(5X,' Jets (JETS)           : eta min,max = ',2F8.1,/,
     1  5X,'                         pt  min,max = ',2F8.1)
 3610 FORMAT(5X,'                        limit of ',I4,
     1  ' Jets above Pt = ',F8.1)
C
 4000 FORMAT(//10X,'f.) Top Selection Cuts :',/)
 4500 FORMAT(5X,' Cuts selection :',/,
     1 15X,' dPhi(Lepton1-Lepton2) cut enabled')
 4501 FORMAT(5X,' Cuts selection :',/,
     1 15X,' dPhi(Lepton1-Lepton2) cut disabled')
 4510 FORMAT(15X,' Dilepton Mass Window cut enabled')
 4511 FORMAT(15X,' Dilepton Mass Window cut disabled')
 4520 FORMAT(15X,' dPhi vs Dilepton Mass Window cut enabled')
 4521 FORMAT(15X,' dPhi vs Dilepton Mass Window cut disabled')
 4530 FORMAT(15X,' Mt(lepton-photon-Etmiss) Window cut enabled')
 4531 FORMAT(15X,' Mt(lepton-photon_Etmiss) Window cut disabled')
 4540 FORMAT(15X,' Muon Energy(Isolation-Core) Cone cuts enabled')
 4541 FORMAT(15X,' Muon Energy(Isolation-Core) Cone cut disabled')
 4010 FORMAT(/5X,' Dilepton Finder Cuts : ',/,
     1  5X,' Leptons : ',/,
     2 15X,     ' Isolated Electron Etmin                 = ',F8.1,/,
     3 15X,     ' Isolated Muon Ptmin                     = ',F8.1,/,
     4 15X,     ' Isolated Photon Etmin                   = ',F8.1)
 4011 FORMAT(5X,' Muon Isolation Cuts : ',/,
     1 15X,     ' Core Cone Size                          = ',F8.2,/,
     2 15X,     ' Isolation Cone Size                     = ',F8.2,/,
     3 15X,     ' CF - Max E(Isolation-Core)              = ',F8.1,/,
     4 15X,     ' EF - Max E(Isolation-Core)              = ',F8.1,/,
     5 15X,     ' Min dR(Muon-nearest jet)                = ',F8.1)
 4012 FORMAT(5X,' Electron/Photon Isolation Cuts :',/,
     1 15X,     ' Min dR(Electron-nearest jet)            = ',F8.1,/,
     2 15X,     ' Min dR(Photon-nearest jet)              = ',F8.1)
 4013 FORMAT(5X,' Photon Bremsstrahlung Cuts :',/,
     1 15X,     ' Cobrem Flag (1/-1 for reject/keep brem) = ',I8,/,
     2 15X,     ' Min dR(Muon-electron/photon)            = ',F8.1,/,
     3 15X,     ' WAbrem Flag (1/-1 for reject/keep brem) = ',I8,/,
     4 15X,     ' Min Etmiss for W WAB finding            = ',F8.1,/,
     5 15X,     ' Max dR(Lepton-Photon) for W WAB finding = ',F8.1,/,
     6 15X,     ' Min / Max Mt(lep-gam-Etmiss) (window)   = ',2F8.1)
 4014 FORMAT(5X,' Lepton Correllation Cuts :',/,
     1 15X,     ' Max dPhi(Lepton1-Lepton2) in degrees    = ',F8.1,/,
     2 15X,     ' Min / Max Dilepton Mass (window cut)    = ',2F8.1)
 4015 FORMAT(5X,' Missing Et Cuts : ',/,
     1 15X,     ' Etmin (Calorimters+ICD+MG)              = ',F8.1,/,
     2 15X,     ' Etmin (Muon Corrected)                  = ',F8.1)
 4016 FORMAT(5X,' Jet Cuts : ',/,
     1 15X,     ' Min no. of Jets                         = ',I8,/,
     2 15X,     ' Etmin Jet 1                             = ',F8.1,/,
     3 15X,     ' Etmin Jet 2                             = ',F8.1,/)
C
 4020 FORMAT(5X,' Lepton+Jets Finder Cuts : ',/,
     1  5X,' Leptons : ',/,
     2 15X,     ' Ptmin Muon                            = ',F8.1,/,
     3 15X,     ' Min no. of Muons                      = ',I8,/,
     4 15X,     ' Max no. of Muons                      = ',I8,/,
     5 15X,     ' Min DPhi between Jet1 and Neg_Phi_Mu1 = ',F8.1,/,
     6 15X,     ' Ptmin Electron                        = ',F8.1,/,
     7  5X,' Missing Et Cuts : ',/,
     8 15X,     ' Etmin (Calorimters+ICD+MG)            = ',F8.1,/,
     9 15X,     ' Etmin (Muon Corrected)                = ',F8.1,/,
     1  5X,' Jet Cuts : ',/,
     2 15X,     ' Min no. of Jets                       = ',I8,/,
     3 15X,     ' Etmin Jet 1                           = ',F8.1,/,
     4 15X,     ' Etmin Jet 2                           = ',F8.1,/,
     5 15X,     ' Etmin Jet 3                           = ',F8.1,/,
     6 15X,     ' Etmin Jet 4                           = ',F8.1,/,
     7 15X,     ' Etmin Jet 5                           = ',F8.1,/,
     8 15X,     ' Etmin Jet 6                           = ',F8.1,/,
     9 15X,     ' Max no. of Jets (n)                   = ',I8,/,
     1 15X,     ' Etmax Jet (n+1)                       = ',F8.1)
 4040 FORMAT(5X,' Primary Muon Isolation Cuts Disabled ')
 4041 FORMAT(5X,' Primary Muon Isolation Cuts Enabled :')
 4042 FORMAT(15X,'PTMin for Isolated muon               = ',F8.1,/,
     1 15X,      'Min number of isolated muons          = ',I8,/, 
     2 15X,      'Max number of isolated muons          = ',I8,/, 
     3 15X,      'PTMin for Non-Isolated muon           = ',F8.1,/,
     & 15X,      'Min number of nonisolated muons       = ',I8,/, 
     4 15X,      'Max number of nonisolated muons       = ',I8,/, 
     5 15X,      'dR min to nearest jet for Iso Muon    = ',F8.1,/,
     6 15X,      'dR Max to nearest jet for NonIso Muon = ',F8.1)
 4043 FORMAT(15X,' Isolation Cone Subtraction Disabled ',/)
 4044 FORMAT(15X,' Isolation Cone Subtraction Enabled :',/)
 4050 FORMAT(5X,' HT Range & Definition : ',/,
     1 15X,     ' HT min                                = ',F8.1,/,
     2 15X,     ' Jet Etmin for HT calculation          = ',F8.1,/,
     3 15X,     ' Jet Etamax for HT calculation         = ',F8.1)
 4051 FORMAT(5X,' Aplanarity of jets                    = ',F8.5)
 4052 FORMAT(15X,'One jet dropped to mimic Jet->iso Mu:',/,
     1 15X,     ' Etmin for dropped jet                 = ',F8.1,/,
     2 15X,     ' Etmax for dropped jet                 = ',F8.1,/,
     3 15X,     ' Etamin for dropped jet                 = ',F8.1,/,
     4 15X,     ' Etamax for dropped jet                 = ',F8.1)
C
 5000 FORMAT(/10X,'g.) W-finder Selection cuts :',//,
     1  5X,' Lepton Cuts : ',/,
     2 15X,     ' Isolated Electron Etmin                 = ',F8.1,/,
     3 15X,     ' Isolated Muon Ptmin                     = ',F8.1,/,
     4 15X,     ' Isolated Photon Etmin                   = ',F8.1,/,
     3  5X,' Missing Et Cuts : ',/,
     4 15X,     ' Etmin (Calorimters+ICD+MG)            = ',F8.1,/,
     5 15X,     ' Etmin (Muon Corrected)                = ',F8.1)
 5020 FORMAT(5X,' Isolation cuts :',/,
     1 15X,' Max dR conesize for nearside jet axis tag  = ',F8.1,/,
     2 15X,' No.of sigma for dR=0.6 cone tag            = ',F8.1)
C
 5030 FORMAT(/10X,'h.) Z-finder Selection cuts :',//,
     1  5X,' Lepton Cuts : ',/,
     2 15X,     ' Ptmin lepton1                         = ',F8.1,/,
     3 15X,     ' Ptmin lepton2                         = ',F8.1)
C
 5040 FORMAT(/10X,'i.) WWgamma-finder Selection cuts :',//,
     1  5X,' Lepton Cuts : ',/,
     2 15X,     ' Ptmin lepton                          = ',F8.1,/,
     3 15X,     ' Etmin photon                          = ',F8.1,/,
     4  5X,' Missing Et Cuts : ',/,
     5 15X,     ' Etmin (Calorimters+ICD+MG)            = ',F8.1,/,
     6 15X,     ' Etmin (Muon Corrected)                = ',F8.1,/,
     7  5X,' Cluster id cuts : ',/,
     8 15X,     ' Min Mt(lepton+photon+neutrino)        = ',F8.2,/,
     9 15X,     ' Min dR(lepton-photon)                 = ',F8.2)
 5050 FORMAT(5X,' Isolation cuts :',/,
     1 15X,'      Min  dR lepton - jet                  = ',F8.1,/,
     2 15X,'      No.of sigma for dR=0.6 cone tag       = ',F8.1,/,
     3 15X,'      Min  dR photon - jet                  = ',F8.1)
 5060 FORMAT(5X,' DPHI lepton-photon cut :',/,
     1 15X,'      Max dphi lepton - photon              = ',F8.1)
 5070 FORMAT(5X,' Lepton - Photon mass window cut :',/,
     1 15X,'      Min  mass lepton - photon             = ',F8.1,/,
     2 15X,'      Max  mass lepton - photon             = ',F8.1)
 6000 FORMAT(1H1)
      END
