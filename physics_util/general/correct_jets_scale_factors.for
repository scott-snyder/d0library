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
C-      ** Note: errors are from fit parameter errors (ie. error(i,1)), and
C-          estimates of systematic errors (ie. error(i,2)).  the first index
C-          indicates positive (i=1) error and negative error (i=2).
C-
C-   Controls:    QCD_JET_CORRECTION_RCP
C-
C-   Created  10-AUG-1995   Bob Kehoe, Rich Astur
C-   Updated  Oct-11-1995   Bob Kehoe   -- provide cc_boost for d0fix
C-   Updated  Dec-13-1995   Bob Kehoe   -- correct ICD bug
C----------------------------------------------------------------------

      IMPLICIT NONE
      INTEGER istat,i,version,ipara,ietad,lrcp,ier,icone,runno
      INTEGER zebra_override,no_zebra,run_number,runnum
      SAVE no_zebra,run_number
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
      REAL response,error(2,2),off_factor,cc_boost,cryo_sys
      REAL et_in, et_used, icdfrac_used, etad_used,icd_sys
      REAL emfrac_used,chfrac_used,icd_error,det_scale
      REAL e_corr_icd(-42:42),e_corr_icd_off(10:13,5),etpara(5,3)
      LOGICAL ok,first
      DATA first /.true./

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
          IF (ier.EQ.0) CALL ezget('E_CORR_ICD',e_corr_icd,ier)
          IF (ier.EQ.0) CALL ezget('E_CORR_ICD_OFF',e_corr_icd_off,ier)
          IF (ier.EQ.0) CALL ezget('ETPARA',etpara,ier)
          IF (ier.EQ.0) CALL ezget('icd_sys',icd_sys,ier)
          IF (ier.EQ.0) CALL ezget('cc_boost',cc_boost,ier)
          IF (ier.EQ.0) CALL ezget('cryo_sys',cryo_sys,ier)
          IF (ier.EQ.0) CALL ezget('ecn_factor',ecn_factor,ier)
          IF (ier.EQ.0) CALL ezget('ecn_error',ecn_error,ier)
          IF (ier.EQ.0) CALL ezget('ecs_factor',ecs_factor,ier)
          IF (ier.EQ.0) CALL ezget('ecs_error',ecs_error,ier)
          CALL ezrset
          IF (ier.NE.0) THEN
            CALL errmsg('RCP error','correct_jets_scale_factors',
     &        'Read error:abort ','F')
            ier = -3
            GOTO 999
          ENDIF
        ELSE
          CALL errmsg('NO QCD_JET_CORRECTION_RCP',
     &      'correct_jets_scale_factors','NO RCP file to work with','F')
          ier = -2
          GOTO 999
        ENDIF
        first = .false.
      ENDIF
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

C-      *** Factor in cryostat response factors.  CC boost should be taken
C-      *** as 1.0 for non-d0fixed data ***
      r_ccem = cc_boost
      r_ccfh = cc_boost
      r_ccch = cc_boost
      r_ecnem = cc_boost/ecn_factor
      r_ecnfh = cc_boost/ecn_factor
      r_ecnch = cc_boost/ecn_factor
      r_ecsem = cc_boost/ecs_factor
      r_ecsfh = cc_boost/ecs_factor
      r_ecsch = cc_boost/ecs_factor
      er_ccem_st = 0.
      er_ccfh_st = 0.
      er_ccch_st = 0.
      er_ecnem_st = ecn_error
      er_ecnfh_st = ecn_error
      er_ecnch_st = ecn_error
      er_ecsem_st = ecs_error
      er_ecsfh_st = ecs_error
      er_ecsch_st = ecs_error
      er_ccem_sy = 0.
      er_ccfh_sy = 0.
      er_ccch_sy = 0.
      er_ecnem_sy = cryo_sys
      er_ecnfh_sy = cryo_sys
      er_ecnch_sy = cryo_sys
      er_ecsem_sy = cryo_sys
      er_ecsfh_sy = cryo_sys
      er_ecsch_sy = cryo_sys
      icd_error = 0.
      IF (version.LT.12) THEN        ! No correction for version 12 on
C-        *** Apply HV correction to run 1a data reco 10 ***
        IF ( version .GE. 8 .AND. version .LE. 10 ) THEN
          IF (no_zebra.EQ.0) run_number = runno()
          IF (run_number.GE.52470) THEN
            call errmsg('runs>52470 have lower cal HV (2.0kV) ',
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
        else
          call errmsg('RECO 11 and higher have HV corrections ',
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
      else
        call errmsg('RECO 11 and higher have HV corrections ',
     &    'correct_jets_scale_factors','turn off HV corr.','W')
        call errmsg('RECO 12 has new CAL+ICD sampling frac. ',
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
      det_scale = factor*response

  999 RETURN

      ENTRY scale_factors_runs(zebra_override,runnum)
C------------------------------------------------------------------------
C-    Purpose:  input value of run number when running from ntuples
C-
C-    inputs:
C-          zebra_override  I  =1 for ntuples, =0 for zebra files
C-          runnum          I  run number of event
C------------------------------------------------------------------------
      no_zebra = zebra_override
      run_number = runnum
      RETURN
      END
