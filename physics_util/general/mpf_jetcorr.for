      SUBROUTINE mpf_jetcorr(old_jet_energy,det_eta,algorithm,
     &  response,error,istat)

C----------------------------------------------------------------------
C-   Purpose and Methods : to return value of jet response and its error for
C-        jets of different algorithms.
C-
C-   Inputs  :
C-              old_jet_energy  R   -- uncorrected jet energy
C-              det_eta         R   -- detector eta of jet
C-              algor           I   -- jet reco algorithm
C-              *** note: algorithms numbered accordingly --> 1 = 0.7, 2 = 0.5,
C-                    3 = 0.3, 4 = NN, 5 = 1.0
C-
C-   Outputs :
C-              response        R   -- jet response (=inverse of correction)
C-              error           R   -- estimated error**
C-
C-      ** Note: errors are from fit parameter errors (ie. error(i,1)), and
C-          estimates of systematic errors (ie. error(i,2)).  the first index
C-          indicates positive (i=1) error and negative error (i=2).
C-
C-   Created  Jul-03-1995  Bob Kehoe
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER npar,nalgo
      PARAMETER (npar = 5)
      PARAMETER (nalgo = 5)
      INTEGER algor,istat,algorithm,lrcp,ier,k,nx,version,index
      integer nbins
      REAL low_e_sys(10),low_e_bin(10),unbias_error(10)
      REAL mpf_e_sys(10),mpf_e_bin(10),systematic(10),interpolation
      REAL rhi7(npar),rhi5(npar),rhi3(npar),rhinn(npar),
     &   rhi10(npar)
      REAL errhi7(npar),errhi5(npar),errhi3(npar),errhinn(npar),
     &   errhi10(npar)
      REAL old_jet_energy,det_eta,val(6,npar),err(6,npar)
      REAL min_jet_energy,response,error(2,2),aone,azero
      real response_high,response_low,cryo_sys,mi_sys
      LOGICAL first,ok,monte_carlo_data,do_bias_correction
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
        IF (ier.EQ.0) THEN
          CALL ezget('DO_BIAS_CORRECTION',do_bias_correction,ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_SYS',0,0,0,nbins,ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_SYS',1,nbins,1,low_e_sys,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_BIN',1,nbins,1,low_e_bin,
     &      ier)
          IF (ier.EQ.0) CALL ezget('cryo_sys',cryo_sys,ier)
          if (ier.eq.0) CALL ezgeta('MPF_E_SYS',0,0,0,nbins,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_E_SYS',1,nbins,1,
     &       mpf_e_sys,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_E_BIN',1,nbins,1,
     &       mpf_e_bin,ier)
          if (ier.eq.0) call ezget('MIN_JET_ENERGY',MIN_JET_ENERGY,IER)
          if (ier.eq.0) CALL ezgeta('MPF_RHI7',0,0,0,nx,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RHI7',1,nx,1,rhi7,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RHI5',1,nx,1,rhi5,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RHI3',1,nx,1,rhi3,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RHINN',1,nx,1,rhinn,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RHI10',1,nx,1,rhi10,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRHI7',1,nx,1,errhi7,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRHI5',1,nx,1,errhi5,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRHI3',1,nx,1,errhi3,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRHINN',1,nx,1,errhinn,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRHI10',1,nx,1,errhi10,ier)
          IF (ier.NE.0) CALL errmsg('rcp error','mpf_jetcorr',
     &      ' ','F')
          CALL ezrset
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'mpf_jetcorr','NO RCP file to work with','F')
        ENDIF
        DO k = 1,nx
          val(1,k) = rhi7(k)
          err(1,k) = errhi7(k)
          val(2,k) = rhi5(k)
          err(2,k) = errhi5(k)
          val(3,k) = rhi3(k)
          err(3,k) = errhi3(k)
          val(4,k) = rhinn(k)
          err(4,k) = errhinn(k)
          val(5,k) = rhi10(k)
          err(5,k) = errhi10(k)
        ENDDO
        first = .false.
        call vzero(unbias_error,5)
        if (.not.do_bias_correction) then
          call ucopy(low_e_sys,unbias_error,nbins)
          if (low_e_bin(1).ne.mpf_e_bin(1)) call errmsg('rcp error',
     &      'mpf_jetcorr','error incorrectly read','W')
        endif
        do k = 1,nbins
          mpf_e_sys(k) = sqrt(mpf_e_sys(k)**2. + unbias_error(k)**2.)
        enddo
      ENDIF
      mi_sys = 0.
      if (abs(det_eta).lt.1.2) mi_sys = cryo_sys
      do k = 1,nbins
        if (mpf_e_bin(k).gt.120.0.and.mpf_e_bin(k).lt.350.) then
          systematic(k) = sqrt(mpf_e_sys(k)**2. + mi_sys**2.)
        else
          systematic(k) = mpf_e_sys(k)
        endif
      enddo
      response = 1.
      CALL vzero(error,2*2)
      istat = 0
      CALL correct_jets_reco_version(version,istat)
      algor = algorithm
      IF (algorithm.EQ.4.AND.version.LE.11) THEN
        algor = 3
        CALL errmsg('v11 NN algorithm','mpf_jetcorr',
     &        ' using 0.3 response corr.','W')
      ELSEIF (algorithm.EQ.4.AND.monte_carlo_data()) THEN
        CALL errmsg('v12 NN algorithm','mpf_jetcorr',
     &        ' using 0.5 response corr. for MC','W')
      ELSEIF (algorithm.EQ.3) THEN
        algor = 1
      ELSEIF (algorithm.EQ.1) THEN
        algor = 3
      ENDIF

C-        *** determine response and base level of error bars ***
      IF (val(algor,1).ne.0.0.and.val(algor,2).ne.0.0.and.
     &      old_jet_energy.gt.0.0) then
        if (old_jet_energy.lt.min_jet_energy) then
          response = val(algor,1) + val(algor,2)*log(min_jet_energy)
        else
          response = val(algor,1) + val(algor,2)*log(old_jet_energy)
        endif
        azero = val(algor,1) + err(algor,1)
        aone = val(algor,2) + err(algor,2)
        response_high = azero + aone*log(old_jet_energy)
        azero = val(algor,1) - err(algor,1)
        aone = val(algor,2) - err(algor,2)
        response_low = azero + aone*log(old_jet_energy)
        error(1,1) = abs(response - response_high)
        error(2,1) = abs(response - response_low)
        index = 0
        DO k = 1,nbins
          IF (old_jet_energy.GT.mpf_e_bin(k)) index = k
        ENDDO
        IF (index.EQ.0) THEN
          error(1,2) = systematic(1)
          error(2,2) = systematic(1)
        ELSEIF (index.EQ.nbins) THEN
          error(1,2) = systematic(nbins)
          error(2,2) = systematic(nbins)
        ELSE
          interpolation = (old_jet_energy-mpf_e_bin(index))
     &        /(mpf_e_bin(index+1)-mpf_e_bin(index))
          error(1,2) = systematic(index) +
     &        interpolation*(systematic(index+1)-systematic(index))
          error(2,2) = error(1,2)
        ENDIF
      ELSEif (old_jet_energy.le.0.0) then
        CALL errmsg('zero or negative jet energy','mpf_jetcorr',
     &        ' cant correct jet','W')
        istat = -1
      ELSE
        CALL errmsg('messed up param.','mpf_jetcorr',
     &        'didnt get right #','F')
        istat = -2
      ENDIF

  999 RETURN
      END
