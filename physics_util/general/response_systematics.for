      SUBROUTINE RESPONSE_SYSTEMATICS(jet_e,jet_et,det_eta,algor,
     &  phy_bck_err,sys_bias_error_low,sys_bias_error_high,
     &  kt_err,topo_correction)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : calculate jet scale errors from residual
C-   biases in method and topology correction (used w/ MC data)
C-
C-   Inputs  :
C-                jet_e    R -- jet energy
C-                jet_et   R -- jet ET
C-                det_eta  R -- detector eta of jet
C-                algor    I -- jet reco algorithm
C-              *** note: algorithms follow qcd_jet_correction convention
C-                  1 = 0.3, 2 = 0.5, 3 = 0.7, 4 = NN, 5 = 1.0
C-
C-   Outputs : (response errrors/corrections)
C-                phy_bck_err         R
C-                sys_bias_error_low  R
C-                sys_bias_error_high R
C-                kt_err              R
C-                topo_correction     R  -- for MC data only
C-
C-   Controls:
C-
C-   Created  21-JUL-1997   Bob Hirosky
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      REAL jet_e,jet_et,det_eta
      REAL phy_bck_err,sys_bias_error_low,sys_bias_error_high,kt_err
      REAL topo_correction
      INTEGER algor
C
      INTEGER maxbin, nbins
      PARAMETER (maxbin = 15)
      REAL MPF_ERR_BIN(maxbin)
      REAL RESIDUAL_BIAS_ERROR(maxbin), CAL_LEAK_ERROR(maxbin),
     &  PHY_BCK_ERROR, KT_ERROR(maxbin)
      SAVE MPF_ERR_BIN
      SAVE RESIDUAL_BIAS_ERROR, CAL_LEAK_ERROR, PHY_BCK_ERROR, KT_ERROR
C
      INTEGER npar, nalgo, mc_rcp, par_offset
      PARAMETER (npar = 5, nalgo = 5)
      REAL MC_TOPO_PAR(NPAR*NALGO)
      SAVE MC_TOPO_PAR, MC_RCP
C
      REAL mbias_err, punch_err, divdif, a1, a2, a3, a4, a5
C
      INTEGER lrcp, ier
C
      LOGICAL first, ok
      DATA first /.true./
C----------------------------------------------------------------------
      IF (first) THEN
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF(ier.NE.0) THEN
            CALL errmsg('RCP not found','MPF_jetcorr',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF                 ! *** read in RCP parameters ***
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.NE.0) THEN
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'mpf_jetcorr','NO RCP file to work with','F')
        ENDIF
        CALL ezget('PHY_BCK_ERROR',PHY_BCK_ERROR,IER)
        IF (ier.EQ.0) CALL ezgeta('MPF_ERR_BIN',0,0,0,nbins,ier)
        IF (ier.EQ.0) CALL ezgeta('MPF_ERR_BIN',1,nbins,1,mpf_err_bin,
     &      ier)
        IF (ier.EQ.0) CALL ezgeta('RESIDUAL_BIAS_ERROR',
     &    1,nbins,1,RESIDUAL_BIAS_ERROR,ier)
        IF (ier.EQ.0) CALL ezgeta('CAL_LEAK_ERROR',
     &    1,nbins,1,CAL_LEAK_ERROR,ier)
        IF (ier.EQ.0) CALL ezgeta('KT_ERROR',
     &    1,nbins,1,KT_ERROR,ier)
        CALL ezget('MC_RCP',mc_rcp, ier )
        IF (MC_RCP.EQ.1) THEN
          IF (ier.EQ.0) CALL ezget('MC_TOPO_PAR',mc_topo_par,ier)
        ENDIF
        IF (ier.NE.0)
     &      CALL errmsg('rcp read error','mpf_jetcorr',' ','F')
        CALL ezrset
        first = .false.
      ENDIF !(first)
c
      phy_bck_err = PHY_BCK_ERROR
      mbias_err = divdif(residual_bias_error,mpf_err_bin,nbins,jet_e,1)
      punch_err = divdif(cal_leak_error,mpf_err_bin,nbins,jet_e,1)
      kt_err = divdif(kt_error,mpf_err_bin,nbins,jet_et,1)
      IF (abs(det_eta).GT.0.7) THEN
        punch_err = punch_err * (2 - abs(det_eta))/1.3
        punch_err = max(0.0,punch_err)
      ENDIF
      sys_bias_error_low = mbias_err              ! low sys bias error
      sys_bias_error_high = mbias_err + punch_err ! high sys bias error
C
      topo_correction = 1.0
      IF (MC_RCP.EQ.1) THEN   ! find MC topo correction
        par_offset = (algor-1) * npar
        a1 = mc_topo_par(par_offset+1)
        a2 = mc_topo_par(par_offset+2)
        a3 = mc_topo_par(par_offset+3)
        a4 = mc_topo_par(par_offset+4)
        a5 = mc_topo_par(par_offset+5)
        topo_correction = (1-(a1+a2*exp(a3*(a4+a5*jet_e))))
      ENDIF
c
  999 RETURN
      END
