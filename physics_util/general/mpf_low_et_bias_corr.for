      SUBROUTINE mpf_low_et_bias_corr(old_jet_et,det_eta,algorithm,
     &  response,error,istat)
C----------------------------------------------------------------------
C-   Purpose and Methods : to return value of relative response of jets in
C-        region biased by reconstruction threshold.  Response and errors
C-        for jets of different algorithms are returned.
C-
C-   Inputs  :
C-              old_jet_et      R   -- uncorrected jet et
C-              det_eta         R   -- detector eta of jet
C-              algorithm       I   -- jet reco algorithm
C-              *** note: algorithms follow qcd_jet_correction convention
C-                  1 = 0.3, 2 = 0.5, 3 = 0.7, 4 = NN, 5 = 1.0
C-
C-   Outputs :
C-              response        R   -- jet response (=inverse of correction)
C-              error(2,2)      R   -- estimated error** (positive and neg.)
C-              istat           I   -- = 1 if error
C-
C-      ** Note: errors are described by their effect on corrected jet energy
C-      ** error(1,1) = low statistical/uncorrelated error
C-      ** error(2,1) = high statistical/uncorrelated error
C-      ** error(1,2) = low systematic/correlated error
C-      ** error(2,2) = high systematic/correlated error
C-
C-   Created  Jul-03-1995  Bob Kehoe
C-   Updated  Oct-27-1995  Bob Kehoe  -- fix errors
C-   Updated   5-FEB-1997  Bob Hirosky -- update for CAFIX 5.1
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER npar,nalgo
      PARAMETER (npar = 2)
      PARAMETER (nalgo = 5)
      INTEGER algor,istat,algorithm,lrcp,ier,k
      REAL low_et_sys(10),low_et_bin(10),interpolation
      REAL bias7(npar),bias7_min(npar),bias7_max(npar)
      REAL bias5(npar),bias5_min(npar),bias5_max(npar)
      REAL bias3(npar),bias3_min(npar),bias3_max(npar)
      REAL bias1(npar),bias1_min(npar),bias1_max(npar)
C
      REAL old_jet_et,det_eta
      REAL val(nalgo,npar),val_l(nalgo,npar),val_h(nalgo,npar),max_bias
      SAVE val,val_h,val_l,max_bias
      REAL error(2,2)
      REAL aone,azero
      REAL response, response_low, response_high
      LOGICAL first,ok
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
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS7',bias7,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS7_MIN',bias7_min,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS7_MAX',bias7_max,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS5',bias5,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS5_MIN',bias5_min,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS5_MAX',bias5_max,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS3',bias3,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS3_MIN',bias3_min,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS3_MAX',bias3_max,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS1',bias1,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS1_MIN',bias1_min,ier)
          IF (ier.EQ.0) CALL ezget_rarr('MPF_BIAS1_MAX',bias1_max,ier)
C
          IF (ier.NE.0) CALL errmsg('rcp error','mpf_low_et_bias_corr',
     &      ' ','F')
          CALL ezrset
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'mpf_low_et_bias_corr','NO RCP file to work with','F')
        ENDIF
        DO k = 1,2
          val(1,k) = bias3(k)      ! 0.3 cone
          val_l(1,k) = bias3_min(k)
          val_h(1,k) = bias3_max(k)
          val(2,k) = bias5(k)      ! 0.5 cone
          val_l(2,k) = bias5_min(k)
          val_h(2,k) = bias5_max(k)
          val(3,k) = bias7(k)      ! 0.7 cone
          val_l(3,k) = bias7_min(k)
          val_h(3,k) = bias7_max(k)
          val(4,k) = bias3(k)      ! nn jet  uses 3.0 cone correction
          val_l(4,k) = bias3_min(k)
          val_h(4,k) = bias3_max(k)
          val(5,k) = bias1(k)      ! 1.0 cone
          val_l(5,k) = bias1_min(k)
          val_h(5,k) = bias1_max(k)
        ENDDO
        first = .false.
      ENDIF
C
      response = 1.
      CALL vzero(error,2*2)
      istat = 0
      algor = algorithm
C
      IF (algorithm.EQ.4) THEN
        CALL errmsg('unknown correction - NN',
     &    'mpf_low_et_bias_corr',
     &    'uses 3.0 cone correction','W')
      ENDIF
C-        *** determine response and base level of error bars ***
      azero = val(algor,1)
      aone = val(algor,2)
      response = 1.0 + exp(azero+aone*old_jet_et)
C
      azero = val_h(algor,1)
      aone =  val_h(algor,2)
      response_high = 1.0 + exp(azero+aone*old_jet_et)
C
      azero = val_l(algor,1)
      aone =  val_l(algor,2)
      response_low = 1.0 + exp(azero+aone*old_jet_et)
C                                             ! fractional response errors
      error(1,2) = response_high/response - 1.0
C                                             ! high bias = lower jet energy
      error(2,2) = 1.0 - response_low/response
C                                             ! low bias = higher jet energy
C
      IF (response.GT.max_bias) response = max_bias
C
C-        *** zero error above biassed region ***
      IF (old_jet_et.GT.20.) CALL vzero(error,2*2)

  999 RETURN
      END
