      LOGICAL FUNCTION TOP_DILEP_ANALYSIS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : ntuple filling routine for top dileptons
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  27-FEB-1994   Meenakshi Narain
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INCLUDE 'D0$INC:PI.DEF'
      include 'd0$inc:top_DILEP_ANALYSIS.INC'
      INTEGER NEVENTS,NMUON_MIN,NELEC_MIN,NELEC_TGHT_MIN,IER
      INTEGER NPHOT_MIN,NPHOT_TGHT_MIN,NJET_MIN
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST / .TRUE. /
C----------------------------------------------------------------------
      top_dilep_analysis = .true.
      nevents = nevents + 1
      IF( first ) THEN
        first = .false.
        CALL EZPICK('top_dilep_rcp')
        IF (IER.EQ.0) CALL EZGET('MIN_MUONS',NMUON_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_ELECTRONS',NELEC_MIN,IER)
        IF (IER.EQ.0)
     &    CALL EZGET('MIN_ELECTRONS_TIGHT',NELEC_TGHT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_PHOTONS',NPHOT_MIN,IER)
        IF (IER.EQ.0)
     &    CALL EZGET('MIN_PHOTONS_TIGHT',NPHOT_TGHT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('MIN_JETS',NJET_MIN,IER)
        CALL EZERR(IER)
      ENDIF
      CALL VZERO(xrun,50)
      CALL VZERO(xelec,nwant_ele*nvar_elec+1)
      CALL VZERO(xphot,nwant_phot*nvar_phot+1)
      CALL VZERO(xjet,nwant_jet*nvar_jet+1)
      CALL VZERO(xmet,nwant_met*nvar_met+1)
      CALL VZERO(xmuon,nwant_muon*nvar_muon+1)
      CALL VZERO(xmuisa,nwant_muisa*nvar_muisa+1)
      CALL VZERO(xbbisa,nwant_bbisa*nvar_bbisa+1)
      CALL VZERO(xl2muon,nwant_l2muon*nvar_l2muon+1)
      CALL VZERO(xl2jet,nwant_l2jet*nvar_l2jet+1)
      CALL VZERO(xglobal,100)
      CALL VZERO(xz_fit,100)
      CALL VZERO(xfilt,50)
      CALL VZERO(yem,20)
C
C ****  event header, trigger/filter information
C
      call fill_evt_geninfo
C
C ****  get ELECTRON information
C
      CALL gtslink('ELE_TGHT', nwant_eletght,ntot_eletght,
     &    elec_link_tght)
      nelec_tght = min(ntot_eletght,nwant_eletght)
      CALL gtslink('ELE_LSE', nwant_ele, ntot_ele, elec_link)
      nelec = min(ntot_ele,nwant_ele)
C
C ****  get PHOTON information
C
      CALL gtslink('GAM_TGHT', nwant_gamtght,ntot_gamtght,
     &    phot_link_tght)
      ngam_tght = min(ntot_gamtght,nwant_gamtght)
      CALL gtslink('GAM_LSE', nwant_phot, ntot_phot, phot_link)
      nphot = min(ntot_phot,nwant_phot)
C
C ****  Get MUON information
C
      CALL gtslink('HARD_MUO', nwant_muon, ntot_muon, muon_link)
      nmuon = min(nwant_muon, ntot_muon)
      CALL gtslink('ISOLMUON', nwant_muon_tght,
     &  ntot_muon_tght, muon_link_tght)
      nmuon_tght = min(nwant_muon, ntot_muon_tght)
C
C ****  Get JET information
C
      CALL gtslink('TOP_JETS', nwant_jet, ntot_jet, jets_link)
      njet = min(nwant_jet, ntot_jet)
C
C ****  Skip the event if
C ****   under minimum number of muons or electrons
C ****
      IF (nelec.LT.nelec_min) THEN
        top_dilep_analysis = .false.
        GOTO 999        ! skip out if none of the electrons
                        ! pass etight or eloose
      ENDIF
      IF (nelec_tght.LT.nelec_tght_min) THEN
        top_dilep_analysis = .false.
        GOTO 999        ! skip out if none of the electrons
                        ! pass etight
      ENDIF
      IF (nphot.LT.nphot_min) THEN
        top_dilep_analysis = .false.
        GOTO 999        ! skip out if none of the photons
                        ! pass gamtight or eloose
      ENDIF
      IF (ngam_tght.LT.nphot_tght_min) THEN
        top_dilep_analysis = .false.
        GOTO 999        ! skip out if none of the photons
                        ! pass gamtight
      ENDIF
      IF (nmuon.LT.NMUON_MIN) THEN
        top_dilep_analysis = .false.
        GOTO 999
      ENDIF
      IF (njet.LT.NJET_MIN) THEN
        top_dilep_analysis = .false.
        GOTO 999
      ENDIF
C
C ****  Fill all relevant information for ELECTRONS
C
      call fill_elec_info
C
C ****  fill PHOTON information in ntuple
C
      call fill_phot_info
C
C ****  Get JETS information
C
      call fill_jets_info
C
C ****  Fill relevant MUON information
C
      call fill_muon_info
C
C ****  Get Missing Et information
C
      call fill_met_info
C
C ****  setup the ele+phot merged array
C
      call fill_global_info
C
C *** Z fitting
C *** using Z_FIT_MUMU (Hobbs)
C
      call fill_zfit_dimuon
C
C *** muon isajet information (if MC events)
C
      call fill_mcinfo
C
C ****  fill trigger/filter/mainring information
C
      call fill_trigfilt_info
C
C ****
C
  950 CONTINUE
C#######################################################################
  999 CONTINUE
      RETURN
      END
