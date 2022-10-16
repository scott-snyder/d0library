      SUBROUTINE correct_jets_scale_factors(et_in,det_eta,e_fract,
     &        cone_size,new_fract,det_scale,error,istat)
C----------------------------------------------------------------------
C-   Purpose and Methods : Apply detector and RECO dependent scale factors.
C-            These involve HV scale factors for pre-RECO 11 calorimeter energy,
C-            sampling weight corrections for ICD/MG and calorimeter energy
C-            for pre-RECO 12 ICR jets, and EC cryostat factors relative to
C-            the CC.
C-
C-   Inputs  :
C-            et_in         R   -- jet Et
C-            det_eta       R   -- detector eta of jet
C-            e_fract(3)    R   -- starting EM, ICD, and CH fractions
C-            cone_size     R   -- cone size used
C-
C-   Outputs :
C-            new_fract(3)  R   -- corrected EM, ICD and CH fractions
C-            det_scale     R   -- overall detector scale factor
C-            error(2,2)    R   -- estimated error** (positive and neg.)
C-            istat         I   -- = 1 if error
C-
C-      ** Note: errors are described by their effect on corrected jet energy
C-      ** error(1,1) = low statistical/uncorrelated error
C-      ** error(2,1) = high statistical/uncorrelated error
C-      ** error(1,2) = low systematic/correlated error
C-      ** error(2,2) = high systematic/correlated error
C-
C-   Controls:    QCD_JET_CORRECTION_RCP
C-
C-   Created  10-AUG-1995   Bob Kehoe, Rich Astur
C-   Updated  Oct-11-1995   Bob Kehoe   -- provide cc_boost for d0fix
C-   Updated  Dec-13-1995   Bob Kehoe   -- correct ICD bug
C-   Updated  Oct-07-1996   Brad Abbott -- Update for CAFIX51
C-                                         Add in Eta Dependence
C-   Updated   8-FEB-1997   Bob Hirosky -- UPDATE ERRORS
C-   Updated  11-FEB-1997   Bob Hirosky    add v11/v12 cryo scales
C-   Updated  20-JUN-1997   Bob Hirosky  -- fix eta dependence error
C-   Updated  21-JUL-1997   Bob Hirosky  -- update eta-dep. error
C-   Updated  10-FEB-1998   Bob Hirosky  -- add SAVE first
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER istat,i,version,ipara,ietad,lrcp,ier,icone,qcdrunno
      INTEGER run_number
      REAL r_ccem,r_ccfh,r_ecsem,r_ecsfh,r_ecnem,r_ecnfh
      REAL r_ccch,r_ecnch,r_ecsch
      REAL er_ccem_st,er_ccfh_st,er_ecsem_st,er_ecsfh_st,er_ecnem_st
      REAL er_ecnfh_st,er_ccch_st,er_ecnch_st,er_ecsch_st
      REAL er_ccem_sy,er_ccfh_sy,er_ecsem_sy,er_ecsfh_sy,er_ecnem_sy
      REAL er_ecnfh_sy,er_ccch_sy,er_ecnch_sy,er_ecsch_sy
      REAL hvscale_ccem,hvscale_ecem
      REAL hvscale_cchad,hvscale_echad
      REAL ccem_sample,ccfh_sample,ecem_sample,ecfh_sample
      REAL ccch_sample,ecch_sample
      REAL ccem_sample_err,ccfh_sample_err,ecem_sample_err
      REAL ecfh_sample_err,ccch_sample_err,ecch_sample_err
      REAL factor,e_fract(3),new_fract(3),det_eta,cone_size
      REAL new_emf,new_icdf,new_fhf,new_chf
      REAL ecn_factor,ecs_factor,ecn_error,ecs_error
      SAVE ecn_factor,ecs_factor,ecn_error,ecs_error
      REAL ecn_factorv12,ecs_factorv12,ecn_errorv12,ecs_errorv12
      SAVE ecn_factorv12,ecs_factorv12,ecn_errorv12,ecs_errorv12
      REAL response,error(2,2),off_factor,cc_boost
      REAL et_in, et_used, icdfrac_used, etad_used,icd_sys
      REAL emfrac_used,chfrac_used,icd_error,det_scale
      REAL e_corr_icd(-42:42),e_corr_icd_off(10:13,5),etpara(5,3)
      INTEGER nbins, nbinsmax
      PARAMETER (nbinsmax=10)
      REAL ETA_ERR_BINS(NBINSMAX), ETA_CORR_ERR(NBINSMAX)
      SAVE ETA_ERR_BINS, ETA_CORR_ERR
      REAL eta_error, eta_corr, divdif
      INTEGER MC_RCP
      LOGICAL do_eta_correction
      SAVE do_eta_correction
C
      LOGICAL ok,first
      DATA first /.true./
      SAVE first
c
C----------------------------------------------------------------------
C-        *** Initialize ***
      det_scale = 1.0
      factor  = 1.0
      response = 1.0
      istat = 0
      CALL vzero(error,2*2)
      CALL ucopy(e_fract,new_fract,3)
      IF (first) THEN
        CALL ezloc('QCD_JET_CORRECTION_RCP',lrcp)
        ok = lrcp .GT. 0
        IF (.NOT. ok) THEN
          CALL inrcp('QCD_JET_CORRECTION_RCP',ier)
          IF (ier.EQ.0) CALL ezpick('QCD_JET_CORRECTION_RCP')
          IF (ier.EQ.0) CALL ezerr(ier)
          IF (ier.NE.0) THEN
            CALL errmsg('RCP not found','correct_jets_scale_factors',
     &        'QCD_JET_CORRECTION_RCP','F')
          ENDIF
          CALL ezrset
        ENDIF
        CALL ezpick('QCD_JET_CORRECTION_RCP')
        CALL ezerr(ier)
        IF (ier.EQ.0) THEN    ! *** read in RCP parameters ***
          CALL ezget('HVSCALE_CCEM',hvscale_ccem, ier )
          IF (ier.EQ.0) CALL ezget('HVSCALE_ECEM',hvscale_ecem, ier )
          IF (ier.EQ.0) CALL ezget('HVSCALE_CCHAD',hvscale_cchad, ier )
          IF (ier.EQ.0) CALL ezget('HVSCALE_ECHAD',hvscale_echad, ier )
          IF (ier.EQ.0) CALL ezget('CCEM_SAMPLE',ccem_sample, ier )
          IF (ier.EQ.0) CALL ezget('ECEM_SAMPLE',ecem_sample, ier )
          IF (ier.EQ.0) CALL ezget('CCFH_SAMPLE',ccfh_sample, ier )
          IF (ier.EQ.0) CALL ezget('ECFH_SAMPLE',ecfh_sample, ier )
          IF (ier.EQ.0) CALL ezget('CCCH_SAMPLE',ccch_sample, ier )
          IF (ier.EQ.0) CALL ezget('ECCH_SAMPLE',ecch_sample, ier )
          IF (ier.EQ.0) CALL ezget('CCEM_SAMPLE_ERR',ccem_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget('ECEM_SAMPLE_ERR',ecem_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget('CCFH_SAMPLE_ERR',ccfh_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget('ECFH_SAMPLE_ERR',ecfh_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget('CCCH_SAMPLE_ERR',ccch_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget('ECCH_SAMPLE_ERR',ecch_sample_err,
     &      ier )
          IF (ier.EQ.0) CALL ezget_rarr('E_CORR_ICD',e_corr_icd,ier)
          IF (ier.EQ.0) CALL ezget_rarr('E_CORR_ICD_OFF',e_corr_icd_off
     &         ,ier)
          IF (ier.EQ.0) CALL ezget_rarr('ETPARA',etpara,ier)
          IF (ier.EQ.0) CALL ezget('icd_sys',icd_sys,ier)
          IF (ier.EQ.0) CALL ezget('cc_boost',cc_boost,ier)
C
          IF (ier.EQ.0) CALL ezget('ecn_factor',ecn_factor,ier)
          IF (ier.EQ.0) CALL ezget('ecn_error',ecn_error,ier)
          IF (ier.EQ.0) CALL ezget('ecs_factor',ecs_factor,ier)
          IF (ier.EQ.0) CALL ezget('ecs_error',ecs_error,ier)
C
          CALL ezget_i('MC_RCP',mc_rcp, ier )
          IF (MC_RCP.EQ.0) THEN
            IF (ier.EQ.0) CALL ezget('ecn_factorv12',ecn_factorv12,ier)
            IF (ier.EQ.0) CALL ezget('ecn_errorv12',ecn_errorv12,ier)
            IF (ier.EQ.0) CALL ezget('ecs_factorv12',ecs_factorv12,ier)
            IF (ier.EQ.0) CALL ezget('ecs_errorv12',ecs_errorv12,ier)
          ELSE
            ecn_factorv12 = ecn_factor
            ecn_errorv12 = ecn_error
            ecs_factorv12 = ecs_factor
            ecs_errorv12 = ecs_error
          ENDIF
C
          CALL ezget_l('DO_ETA_CORRECTION',do_eta_correction,ier)
          IF (ier.EQ.0.AND.do_eta_correction) THEN
            CALL errmsg('doing eta dependent correction',
     &          'correct_jets_scale_factors',
     &          'correct jets near ICR','I')
            CALL ezgeta_i('ETA_ERR_BINS',0,0,0,nbins,ier)
            IF (ier.EQ.0) CALL ezgeta('ETA_ERR_BINS',1,nbins,1,
     &          ETA_ERR_BINS,ier)
            IF (ier.EQ.0) CALL ezgeta('ETA_CORR_ERR',1,nbins,1,
     &          ETA_CORR_ERR,ier)
          ENDIF
          IF (ier.NE.0) THEN
            CALL errmsg('RCP error','correct_jets_scale_factors',
     &        'Read error:abort ','F')
            ier = -3
            CALL ezrset
            GOTO 999
          ENDIF
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'correct_jets_scale_factors','NO RCP file to work with','F')
          ier = -2
          CALL ezrset
          GOTO 999
        ENDIF
        CALL ezrset
        first = .false.
      ENDIF !(first)
C
      CALL correct_jets_reco_version(version,ier)
      icone = 1
      IF (cone_size.LE.0.6) THEN
        icone = 2
        IF (cone_size.LE.0.4) icone = 3
      ENDIF
      et_used       = max(et_in,0.)
      emfrac_used  = max( min(e_fract(1), 1. ), 0. )
      icdfrac_used  = max( min(e_fract(2), 1. ), 0. )
      chfrac_used  = max( min(e_fract(3), 1. ), 0. )
      etad_used     = max( min( det_eta, 4. ), -4. )
C-
C-      *** Factor in cryostat response factors.  CC boost should be taken
C-      *** as 1.0 for non-d0fixed data ***
C-
      r_ccem = cc_boost
      r_ccfh = cc_boost
      r_ccch = cc_boost
      r_ecnem = cc_boost/ecn_factorv12
      r_ecnfh = cc_boost/ecn_factorv12
      r_ecnch = cc_boost/ecn_factorv12
      r_ecsem = cc_boost/ecs_factorv12
      r_ecsfh = cc_boost/ecs_factorv12
      r_ecsch = cc_boost/ecs_factorv12
      er_ccem_st = 0.
      er_ccfh_st = 0.
      er_ccch_st = 0.
      er_ecnem_st = 0.
      er_ecnfh_st = 0.
      er_ecnch_st = 0.
      er_ecsem_st = 0.
      er_ecsfh_st = 0.
      er_ecsch_st = 0.
      er_ccem_sy = 0.
      er_ccfh_sy = 0.
      er_ccch_sy = 0.
      er_ecnem_sy = ecn_errorv12
      er_ecnfh_sy = ecn_errorv12
      er_ecnch_sy = ecn_errorv12
      er_ecsem_sy = ecs_errorv12
      er_ecsfh_sy = ecs_errorv12
      er_ecsch_sy = ecs_errorv12
      icd_error = 0.
C
      IF (version.LT.12) THEN        ! No correction for version 12 on
C-   use run 1a cryo factor
        r_ecnem = cc_boost/ecn_factor
        r_ecnfh = cc_boost/ecn_factor
        r_ecnch = cc_boost/ecn_factor
        r_ecsem = cc_boost/ecs_factor
        r_ecsfh = cc_boost/ecs_factor
        r_ecsch = cc_boost/ecs_factor
        er_ecnem_sy = ecn_error
        er_ecnfh_sy = ecn_error
        er_ecnch_sy = ecn_error
        er_ecsem_sy = ecs_error
        er_ecsfh_sy = ecs_error
        er_ecsch_sy = ecs_error
C-        *** Apply HV correction to run 1a data reco 10 ***
        IF ( version .GE. 8 .AND. version .LE. 10 ) THEN
          run_number = qcdrunno()
          IF (run_number.GE.52470) THEN
            CALL errmsg('runs>52470 have lower cal HV (2.0kV) ',
     &           'correct_jets_scale_factors',
     &           'apply HV corr in RECO<11','W')
            r_ccem = r_ccem*hvscale_ccem
            r_ccfh = r_ccfh*hvscale_cchad
            r_ccch = r_ccch*hvscale_cchad
            r_ecsem = r_ecsem*hvscale_ecem
            r_ecsfh = r_ecsfh*hvscale_echad
            r_ecsch = r_ecsch*hvscale_echad
            r_ecnem = r_ecnem*hvscale_ecem
            r_ecnfh = r_ecnfh*hvscale_echad
            r_ecnch = r_ecnch*hvscale_echad
          ENDIF
        ELSE
          CALL errmsg('RECO 11 and higher have HV corrections ',
     &      'correct_jets_scale_factors','turn off HV corr.','W')
        ENDIF
C-        *** Apply sampling corrections to pre-v12 data ***
        r_ccem = r_ccem*ccem_sample
        r_ccfh = r_ccfh*ccfh_sample
        r_ccch = r_ccch*ccch_sample
        r_ecsem = r_ecsem*ecem_sample
        r_ecsfh = r_ecsfh*ecfh_sample
        r_ecsch = r_ecsch*ecch_sample
        r_ecnem = r_ecnem*ecem_sample
        r_ecnfh = r_ecnfh*ecfh_sample
        r_ecnch = r_ecnch*ecch_sample
        er_ccem_st = ccem_sample_err
        er_ccfh_st = ccfh_sample_err
        er_ccch_st = ccch_sample_err
        er_ecnem_st = sqrt(er_ecnem_st**2.+ecem_sample_err**2.)
        er_ecnfh_st = sqrt(er_ecnfh_st**2.+ecfh_sample_err**2.)
        er_ecnch_st = sqrt(er_ecnch_st**2.+ecch_sample_err**2.)
        er_ecsem_st = sqrt(er_ecsem_st**2.+ecem_sample_err**2.)
        er_ecsfh_st = sqrt(er_ecsfh_st**2.+ecfh_sample_err**2.)
        er_ecsch_st = sqrt(er_ecsch_st**2.+ecch_sample_err**2.)
C-        *** Correct for ICD/MG -- determine detector ETA bin, determine
C-        *** factor (different for IETAD between 10 and 13), interpolate in Et
        ipara = 5                 ! Determine Et bin
        DO i= 4, 1, -1
          IF ( et_used .LE. etpara(i,icone) ) ipara = i
        ENDDO
        ietad = int( abs(etad_used)/.1) + 1
        IF ( ietad .GE. 11 .AND. ietad .LE. 15 ) THEN
          icd_error = icd_sys
        ENDIF
        IF ( ietad .GE. 10 .AND. ietad .LE. 13 ) THEN
          IF ( ipara .GT. 1 .AND. ipara .LT. 5 ) THEN
            off_factor = e_corr_icd_off(ietad, ipara-1) +
     &            (et_used-etpara(ipara-1,icone)) *
     &            ( e_corr_icd_off(ietad,ipara) -
     &            e_corr_icd_off(ietad,ipara-1) )
     &            /( etpara(ipara,icone)-etpara(ipara-1,icone) )
          ELSE
            off_factor = e_corr_icd_off(ietad, ipara )
          ENDIF
          factor = (1.0 - (e_corr_icd(ietad)+off_factor)
     &          *icdfrac_used)
        ELSE
          factor = (1.0 - (e_corr_icd(ietad)*icdfrac_used))
        ENDIF
      ELSE
        CALL errmsg('RECO 11 and higher have HV corrections ',
     &    'correct_jets_scale_factors','turn off HV corr.','W')
        CALL errmsg('RECO 12 has new CAL+ICD sampling frac. ',
     &    'correct_jets_scale_factors','turn off sampling corr.','W')
      ENDIF         ! *** recalculate energy fractions ***
      new_emf = e_fract(1)/factor
      new_icdf = ((e_fract(2) - 1.0)/factor) + 1.0
      new_chf = e_fract(3)/factor
C
C-        *** correct for calorimeter cryostat factors ***
C-        *** determine response and base level of error bars ***
C
      new_fhf = 1.0 - new_emf - new_icdf - new_chf
C
      IF (abs(det_eta).GT.0.8) THEN
        IF (det_eta.LT.0.) THEN
          error(1,1) = er_ecsch_st*new_chf
          error(1,2) = er_ecsch_sy*new_chf
          new_chf = new_chf*r_ecsch
        ELSE
          error(1,1) = er_ecnch_st*new_chf
          error(1,2) = er_ecnch_sy*new_chf
          new_chf = new_chf*r_ecnch
        ENDIF
        IF (abs(det_eta).GT.1.2) THEN
          IF (det_eta.LT.0.) THEN
            error(1,1) = error(1,1) + er_ecsfh_st*new_fhf
            error(1,2) = error(1,2) + er_ecsfh_sy*new_fhf
            new_fhf = new_fhf*r_ecsfh
          ELSE
            error(1,1) = error(1,1) + er_ecnfh_st*new_fhf
            error(1,2) = error(1,2) + er_ecnfh_sy*new_fhf
            new_fhf = new_fhf*r_ecnfh
          ENDIF
          IF (abs(det_eta).GT.1.5) THEN
            IF (det_eta.LT.0.) THEN
              error(1,1) = error(1,1) + er_ecsem_st*new_emf
              error(1,2) = error(1,2) + er_ecsem_sy*new_emf
              new_emf = new_emf*r_ecsem
            ELSE
              error(1,1) = error(1,1) + er_ecnem_st*new_emf
              error(1,2) = error(1,2) + er_ecnem_sy*new_emf
              new_emf = new_emf*r_ecnem
            ENDIF
          ELSE
            error(1,1) = error(1,1) + er_ccem_st*new_emf
            error(1,2) = error(1,2) + er_ccem_sy*new_emf
            new_emf = new_emf*r_ccem
          ENDIF
        ELSE
          error(1,1) = error(1,1) + er_ccem_st*new_emf +
     &            er_ccfh_st*new_fhf
          error(1,2) = error(1,2) + er_ccem_sy*new_emf +
     &            er_ccfh_sy*new_fhf
          new_emf = new_emf*r_ccem
          new_fhf = new_fhf*r_ccfh
        ENDIF
      ELSE
        error(1,1) = error(1,1) + er_ccem_st*new_emf +
     &          er_ccfh_st*new_fhf + er_ccch_st*new_chf
        error(1,2) = error(1,2) + er_ccem_sy*new_emf +
     &          er_ccfh_sy*new_fhf + er_ccch_sy*new_chf
        new_emf = new_emf*r_ccem
        new_fhf = new_fhf*r_ccfh
        new_chf = new_chf*r_ccch
      ENDIF
      error(2,1) = error(1,1)
      error(1,2) = sqrt(error(1,2)**2.+icd_error**2.)
      error(2,2) = error(1,2)
      response = new_emf + new_chf + new_fhf + new_icdf
      new_fract(1) = new_emf/response
      new_fract(2) = new_icdf/response
      new_fract(3) = new_chf/response
C
C***     CORRECT FOR ETA DEPENDENCE OF ENERGY SCALE
C
      eta_corr = 1.0
      IF (do_eta_correction) THEN
        CALL eta_dependence(cone_size,Et_in*response,Det_eta,eta_corr)
C
        eta_error = divdif(ETA_CORR_ERR,ETA_ERR_BINS,
     &    nbins,abs(det_eta),1)
C
        error(1,1) = sqrt (error(1,1)**2 + eta_error**2)
        error(2,1) = sqrt (error(2,1)**2 + eta_error**2)
      ENDIF

      det_scale = factor*response*eta_corr

  999 RETURN
      END
