      SUBROUTINE mpf_jetcorr(old_jet_energy,det_eta,algor,
     &  response,error,istat)
C----------------------------------------------------------------------
C-   Purpose and Methods : to return value of jet response and its error for
C-        jets of different algorithms.
C-
C-   Inputs  :
C-              old_jet_energy  R   -- uncorrected jet energy
C-              det_eta         R   -- detector eta of jet
C-              algor           I   -- jet reco algorithm
C-              *** note: algorithms follow qcd_jet_correction convention
C-                  1 = 0.3, 2 = 0.5, 3 = 0.7, 4 = NN, 5 = 1.0
C-
C-   Outputs :
C-              response        R   -- jet response (=inverse of correction)
C-              error           R   -- estimated error**
C-
C-      ** Note: errors are described by their effect on corrected jet energy
C-      ** error(1,1) = low statistical/uncorrelated error
C-      ** error(2,1) = high statistical/uncorrelated error
C-      ** error(1,2) = low systematic/correlated error
C-      ** error(2,2) = high systematic/correlated error
C-
C-   Created  Jul-03-1995  Bob Kehoe
C-   Updated   6-FEB-1997   Bob Hirosky   -- update for cafix 5.1
C-                             change parametrization to a+b*ln(E)+c*ln(E)**2
C-                             add new errors
C-   Updated  22-APR-1997   Bob Hirosky  -- allow variable bin number for errors
C-   Updated  23-JUL-1997   Bob Hirosky  -- use quadratic interp. for errors
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER npar, nalgo, maxbin
      PARAMETER (npar = 3, nalgo = 5)
      PARAMETER (maxbin = 15)
      INTEGER nbins,istat,algor,lrcp,ier,k,index
C
      REAL MPF_ERR_BIN(maxbin)
      REAL MPF_RES(NPAR*NALGO), MPF_RESV12(NPAR*NALGO)
      REAL MPF_RES_LOW(MAXBIN*NALGO), MPF_RES_LOWV12(MAXBIN*NALGO)
      REAL MPF_RES_HIGH(MAXBIN*NALGO), MPF_RES_HIGHV12(MAXBIN*NALGO)
      SAVE MPF_ERR_BIN, MPF_RES, MPF_RESV12
      SAVE MPF_RES_LOW, MPF_RES_LOWV12, MPF_RES_HIGH, MPF_RES_HIGHV12
C
      REAL MPF_ERR_LOW(maxbin), MPF_ERR_HIGH(maxbin)
      REAL error(2,2),aone,azero,atwo,mbias_err,punch_err
      SAVE mbias_err,punch_err
C
      INTEGER version, par_offset, err_offset, mc_rcp
      REAL old_jet_energy,det_eta
      REAL min_jet_energy,e_used,response
      REAL response_high,response_low
      REAL divdif
      REAL e1,e2,e3
      LOGICAL first,ok,do_bias_correction
      DATA first/.true./
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
        CALL ezget_l('DO_BIAS_CORRECTION',DO_BIAS_CORRECTION,IER)
        IF (ier.EQ.0) CALL ezget('MIN_JET_ENERGY',MIN_JET_ENERGY,IER)
        IF (ier.EQ.0) CALL ezgeta_i('MPF_ERR_BIN',0,0,0,nbins,ier)
        IF (ier.EQ.0) CALL ezgeta('MPF_ERR_BIN',1,nbins,1,mpf_err_bin,
     &      ier)
        IF (ier.EQ.0) CALL ezget_rarr('MPF_RES',mpf_res,ier)
        IF (ier.EQ.0) CALL ezgeta('MPF_RES_LOW',
     &      1,nbins*nalgo,1,mpf_res_low,ier)
        IF (ier.EQ.0) CALL ezgeta('MPF_RES_HIGH',
     &      1,nbins*nalgo,1,mpf_res_high,ier)
        CALL ezget_i('MC_RCP',mc_rcp, ier )
        IF (MC_RCP.EQ.0) THEN
          IF (ier.EQ.0) CALL ezget_rarr('MPF_RESV12',mpf_resV12,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RES_LOWV12',
     &        1,nbins*nalgo,1,mpf_res_lowv12,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RES_HIGHV12',
     &        1,nbins*nalgo,1,mpf_res_highv12,ier)
        ELSE
          CALL ucopy(mpf_res,mpf_resv12,npar*nalgo)
          CALL ucopy(mpf_res_low,mpf_res_lowv12,maxbin*nalgo)
          CALL ucopy(mpf_res_high,mpf_res_highv12,maxbin*nalgo)
        ENDIF
C
        IF (ier.NE.0)
     &      CALL errmsg('rcp read error','mpf_jetcorr',' ','F')
        CALL ezrset
        first = .false.
        IF (.NOT.do_bias_correction) THEN
          CALL errmsg('unknown response bias',
     &      'mpf_jetcorr','biased data-errors invalid at low ET','W')
        ENDIF
      ENDIF !(first)
C
      CALL vzero(error,2*2)
      istat = 0
C
      IF (algor.EQ.4) THEN
        algor = 1
        CALL errmsg('unsupported NN algorithm','mpf_jetcorr',
     &        ' using 0.3 response corr.','W')
      ENDIF
C
      CALL correct_jets_reco_version(version,ier)
C
      par_offset = (algor-1) * npar     ! find starting points in arrays
      err_offset = (algor-1) * nbins
C
C-        *** determine response and base level of error bars ***
      IF (version.LT.12) THEN
        azero = mpf_res(par_offset+1)
        aone  = mpf_res(par_offset+2)
        atwo  = mpf_res(par_offset+3)
        DO k = 1,nbins
          mpf_err_low(k) = mpf_res_low(err_offset+k)
          mpf_err_high(k) = mpf_res_high(err_offset+k)
        ENDDO
      ELSE
        azero = mpf_resv12(par_offset+1)
        aone  = mpf_resv12(par_offset+2)
        atwo  = mpf_resv12(par_offset+3)
        DO k = 1,nbins
          mpf_err_low(k) = mpf_res_lowv12(err_offset+k)
          mpf_err_high(k) = mpf_res_highv12(err_offset+k)
        ENDDO
      ENDIF
C
      IF (azero.NE.0.0 .AND. aone.NE.0.0 .AND.
     &  old_jet_energy.GT.0.0) THEN
C
        E_USED = MAX(min_jet_energy,old_jet_energy)
C
        response = azero + aone*log(e_used) +
     &      atwo*log(e_used)**2
C
C ****  interpolate fit errors
C
        response_high = divdif(mpf_err_high,mpf_err_bin,nbins,e_used,2)
        response_low = divdif(mpf_err_low,mpf_err_bin,nbins,e_used,2)
C                                            ! fractional response errors
        error(1,2) = response_high/response - 1.0
C                                            ! high repsonse = lower new energy
        error(2,2) = 1.0 - response_low/response
C                                            ! low response = higher new energy
C
      ELSEIF (old_jet_energy.LE.0.0) THEN
        CALL errmsg('zero or negative jet energy','mpf_jetcorr',
     &        'cant correct jet','W')
        istat = -1
      ELSE
        CALL errmsg('messed up param.','mpf_jetcorr',
     &        'didnt get right #','F')
        istat = -2
      ENDIF
C
  999 RETURN
      END
