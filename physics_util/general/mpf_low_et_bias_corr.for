      SUBROUTINE mpf_low_et_bias_corr(old_jet_et,det_eta,algorithm,
     &  response,error,istat)

C----------------------------------------------------------------------
C-   Purpose and Methods : to return value of relative response of jets in region
C-        biased by reconstruction threshold.  Response and errors for jets
C-        of different algorithms are returned.
C-
C-   Inputs  :
C-              old_jet_et      R   -- uncorrected jet et
C-              det_eta         R   -- detector eta of jet
C-              algorithm       I   -- jet reco algorithm
C-              *** note: algorithms numbered accordingly --> 1 = 0.7, 2 = 0.5,
C-                    3 = 0.3, 4 = NN, and 5 = 1.0
C-
C-   Outputs :
C-              response        R   -- jet response (=inverse of correction)
C-              error(2,2)      R   -- estimated error** (positive and neg.)
C-              istat           I   -- = 1 if error
C-
C-      ** Note: errors are from fit parameter errors (ie. error(i,1)), and 
C-          estimates of systematic errors (ie. error(i,2)).  the first index
C-          indicates positive (i=1) error and negative error (i=2).
C-
C-   Created  Jul-03-1995  Bob Kehoe
C-   Updated  Oct-27-1995  Bob Kehoe  -- fix errors
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER npar,nalgo
      PARAMETER (npar = 5)
      PARAMETER (nalgo = 5)
      INTEGER algor,istat,algorithm,lrcp,ier,nx,k,version,index
      integer nbins
      REAL low_et_sys(10),low_et_bin(10),interpolation
      REAL rlow5(npar),rlownn(npar)
      REAL errlow5(npar),errlownn(npar)
      REAL old_jet_et,det_eta,val(6,npar),err(6,npar),aone,azero
      REAL response,error(2,2),max_bias,response_high
      real response_low
      LOGICAL first,ok,monte_carlo_data
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
            CALL errmsg('RCP not found','mpf_low_et_bias_corr',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF                 ! *** read in RCP parameters ***
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN
          CALL ezget('max_bias',max_bias,ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_SYS',0,0,0,nbins,ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_SYS',1,nbins,1,low_et_sys,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('LOW_ET_BIN',1,nbins,1,low_et_bin,
     &      ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RLOW5',0,0,0,nx,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RLOW5',1,nx,1,rlow5,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_RLOWNN',1,nx,1,rlownn,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRLOW5',1,nx,1,errlow5,ier)
          IF (ier.EQ.0) CALL ezgeta('MPF_ERRLOWNN',1,nx,1,errlownn,ier)
          IF (ier.NE.0) CALL errmsg('rcp error','mpf_low_et_bias_corr',
     &      ' ','F')
          CALL ezrset
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'mpf_low_et_bias_corr','NO RCP file to work with','F')
        ENDIF
        DO k = 1,nx
          val(1,k) = rlow5(k)
          err(1,k) = errlow5(k)
          val(2,k) = rlow5(k)
          err(2,k) = errlow5(k)
          val(3,k) = rlow5(k)
          err(3,k) = errlow5(k)
          val(4,k) = rlownn(k)
          err(4,k) = errlownn(k)
          val(5,k) = rlow5(k)
          err(5,k) = errlow5(k)
        ENDDO
        first = .false.
      ENDIF
      response = 1.
      CALL correct_jets_reco_version(version,istat)
      CALL vzero(error,2*2)
      istat = 0
      algor = algorithm
      IF (algorithm.EQ.4.AND.version.le.11) THEN
        algor = 3
      elseIF (algorithm.EQ.4.AND.monte_carlo_data()) then
        CALL errmsg('poorly known MC correction - v12 NN',
     &    'mpf_low_et_bias_corr',
     &    ' MC uses data-based bias correction','W')
      ELSEIF (algorithm.EQ.3) THEN
        algor = 1
      ELSEIF (algorithm.EQ.1) THEN
        algor = 3
      ELSEIF (algorithm.EQ.5) THEN
        CALL errmsg('poorly known correction','mpf_low_et_bias_corr',
     &      ' 1.0 algo uses 0.5 cone correction','W')
      ENDIF

C-        *** determine response and base level of error bars ***
      IF (val(algor,2).ne.0.) then
        azero = val(algor,1)
        aone = val(algor,2)
        response = 1.0 + exp(azero+aone*old_jet_et)
        azero = val(algor,1) + err(algor,1)
        aone = val(algor,2) + err(algor,2)
        response_high = 1.0 + exp(azero+aone*old_jet_et)
        azero = val(algor,1) - err(algor,1)
        aone = val(algor,2) - err(algor,2)
        response_low = 1.0 + exp(azero+aone*old_jet_et)
        error(1,1) = abs(response - response_high)
        error(2,1) = abs(response - response_low)
        IF (response.GT.max_bias) response = max_bias
        index = 0
        DO k = 1,nbins
          IF (old_jet_et.GT.low_et_bin(k)) index = k
        ENDDO
        if (algor.eq.4) then
          if (old_jet_et.lt.10) then
            error(1,2) = 0.022
            error(2,2) = 0.022
          elseif (old_jet_et.lt.15) then
            error(1,2) = 0.01
            error(2,2) = 0.01
          endif
        else
          IF (index.EQ.0) THEN
            error(1,2) = low_et_sys(1)
            error(2,2) = low_et_sys(1)
          elseif (index.eq.nbins) then
            error(1,2) = low_et_sys(nbins)
            error(2,2) = low_et_sys(nbins)          
          ELSE
            interpolation = (old_jet_et-low_et_bin(index))
     &          /(low_et_bin(index+1)-low_et_bin(index))
            error(1,2) = low_et_sys(index) +
     &          interpolation*(low_et_sys(index+1)-low_et_sys(index))
            error(2,2) = error(1,2)
          endif
        ENDIF
      ELSE
        CALL errmsg('messed up param.','mpf_low_et_bias_corr',
     &        ' didnt get right RCP #','F')
        istat = -1
      ENDIF

C-        *** zero error above biassed region ***
      IF (old_jet_et.GT.20.) CALL vzero(error,2*2)

  999 RETURN
      END
