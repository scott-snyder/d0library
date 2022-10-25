      FUNCTION QCD_CORRECTEM(PARTICLE,LCLUS,COR_FACTOR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :INTERIM package to apply corrections to EM
C-                        candidates (PELC or PPHO) reco versions
C-                        greater than 10.
C-
C-   Returned value  : 
C-   Inputs  : PARTICLE (C*1) : Particle indicator 'e' or 'E' for electron
C-                              and 'p' or 'P' for photon.
C-             LCLUS (I)      : Pointer to the bank (PELC or PPHO)
C-   Outputs : COR_FACTOR (R) : Correction factor a an EM object w/o
C-                              overwriting the bank contents.
C-   Controls: QCD_CORRECTEM.RCP
C-
C-   Created  25-MAY-1993   Norman A. Graf
C-   Modified 28-SEP-1993   Stan M. Krzywdzinski
C-                          Do nothing for Monte Carlo data
C-   Updated   5-NOV-1993   Marc Paterno: push links
C-   Updated  11-NOV-1993   Marcel Demarteau: increase size of PELC/PPHO banks
C-   Updated   7-NOV-1994   Marcel Demarteau: handle reco versions and 1b data
C-   Modified 12-SEP-1995   Jaehoon Yu : To just provide the correction
C-                          factor for electron or photon.
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL QCD_CORRECTEM,CORRECTEM_BEGIN,CORRECTEM_END
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INTEGER NUMRUN,NUMEVT,NVER
      INTEGER LCLUS,LCACL,LCASH
      INTEGER VERSION,PASS,IZ
      INTEGER PASS_LO,PASS_HI
      INTEGER EL_IETA,EL_IPHI,IETA_HOT(5),IPHI_HOT(5)
      INTEGER IER,CORR_APPLIED
      INTEGER EXPAND
      PARAMETER(EXPAND = 6)
      INTEGER STATUS,ECORRECT_MASK
      REAL EDPTH(5),PDPTH(5),ENERGY_HOT(5)
      REAL DETA
      REAL ZV,DZ
      REAL ECACL,ECLUS,PHI,THETA
      REAL EM(16),ETA_CORR,ECRACK,DE,ERR_DE
      REAL HV_CORR_FACTOR,ETA_FACTOR,PULSER_CORR_FACTOR
      REAL PIN_FACTOR,PULSER_INSTABILITY_FACTOR(2)
      REAL TMOM_FACTOR,TB_MOMENTUM_FACTOR(2)
      REAL LARTEMP_CORRS(3,2),LARPUR_CORRS(3),CLUST_CORRS(3)
      REAL CCALPHA_CORR,CCZSUP_CORR,CCCLUS_CORR
      REAL LARTEMP_FACTOR,LARPUR_FACTOR,CLUST_FACTOR
      REAL CCALPHA_FACTOR,CCZSUP_FACTOR,CCCLUS_FACTOR
      REAL BOOST_FACTOR
      REAL CCEM_BOOST,ECEMN_BOOST,ECEMS_BOOST
      REAL PHISYM_FACTOR,CSF_FACTOR
      REAL CCEM_PHISYM(64)
      REAL VINFO(3,1)
      LOGICAL DONE
      LOGICAL WANT_HV_CORRS,WANT_PLSR_INSTAB_CORRS,WANT_TBMOM_CORRS
      LOGICAL WANT_PULSER_CORRS,WANT_CCEM_PHISYM,WANT_FIX_CSF
      LOGICAL WANT_CLUST_CORRS
      LOGICAL DO_CC_CRACK,DO_EC_CORRS,DO_HV_CORRS,DO_ETA_CORRS
      LOGICAL DO_PULSER_CORRS,DO_LARTEMP_CORRS,DO_LARPUR_CORRS
      LOGICAL DO_CCALPHA_CORRS,DO_CCZSUP_CORRS,DO_CCCLUS_CORRS
      LOGICAL DO_CCEM_BOOST,DO_ECEMN_BOOST,DO_ECEMS_BOOST
      LOGICAL DO_CCEM_PHISYM,DO_FIX_CSF
      LOGICAL DO_PLSR_INSTAB_CORRS,DO_TBMOM_CORRS
      LOGICAL DO_CLUST_CORRS
      LOGICAL FIRST,MONTE_CARLO,OK
C
C ****  New local variables
C
      CHARACTER*1 PARTICLE
      REAL    COR_FACTOR
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF(first) THEN
        first = .false.
        CALL EZPICK('CORRECTEM_RCP')
        CALL EZGET_i('PASS_LOW',PASS_LO,IER)
        CALL EZGET_i('PASS_HIGH',PASS_HI,IER)
        CALL EZGET_i('ECORRECT_MASK',ECORRECT_MASK,IER)
        CALL EZGET_l('DO_CC_CRACK_CORR',DO_CC_CRACK,IER)
        CALL EZGET_l('DO_EC_CORRS',DO_EC_CORRS,IER)
        CALL EZGET_l('DO_HV_CORRS',WANT_HV_CORRS,IER)
        CALL EZGET_l('DO_ETA_CORRS',DO_ETA_CORRS,IER)
        CALL EZGET_l('DO_LARTEMP_CORRS',DO_LARTEMP_CORRS,IER)
        CALL EZGET_l('DO_LARPUR_CORRS',DO_LARPUR_CORRS,IER)
        CALL EZGET_l('DO_CCALPHA_CORRS',DO_CCALPHA_CORRS,IER)
        CALL EZGET_l('DO_CCZSUP_CORRS',DO_CCZSUP_CORRS,IER)
        CALL EZGET_l('DO_CCCLUS_CORRS',DO_CCCLUS_CORRS,IER)
        CALL EZGET_l('DO_PULSER_CORRS',WANT_PULSER_CORRS,IER)
        CALL EZGET_l('DO_PLSR_INSTAB_CORRS',WANT_PLSR_INSTAB_CORRS,IER)
        CALL EZGET_l('DO_TBMOM_CORRS',WANT_TBMOM_CORRS,IER)
        CALL EZGET_l('DO_CCEM_PHISYM',WANT_CCEM_PHISYM,IER)
        CALL EZGET_l('DO_FIX_CSF',WANT_FIX_CSF,IER)
        CALL EZGET_l('DO_CLUST_CORRS',WANT_CLUST_CORRS,IER)
        CALL EZGET_l('DO_CCEM_BOOST',DO_CCEM_BOOST,IER)
        CALL EZGET_l('DO_ECEMN_BOOST',DO_ECEMN_BOOST,IER)
        CALL EZGET_l('DO_ECEMS_BOOST',DO_ECEMS_BOOST,IER)
C
        CALL EZGET_rarr('PULSER_INSTABILITY_FACTOR'
     &       ,PULSER_INSTABILITY_FACTOR,IER)
        CALL EZGET_rarr('TB_MOMENTUM_FACTOR',TB_MOMENTUM_FACTOR,IER)
        CALL EZGET_rarr('LARTEMP_CORR_FACTOR',LARTEMP_CORRS,IER)
        CALL EZGET_rarr('LARPUR_CORR_FACTOR',LARPUR_CORRS,IER)
        CALL EZGET_rarr('CLUST_CORR_FACTOR',CLUST_CORRS,IER)
        CALL EZGET('CCALPHA_CORR',CCALPHA_CORR,IER)
        CALL EZGET('CCZSUP_CORR',CCZSUP_CORR,IER)
        CALL EZGET('CCCLUS_CORR',CCCLUS_CORR,IER)
        CALL EZGET('CCEM_BOOST',CCEM_BOOST,IER)
        CALL EZGET('ECEMN_BOOST',ECEMN_BOOST,IER)
        CALL EZGET('ECEMS_BOOST',ECEMS_BOOST,IER)
        CALL EZGET_rarr('CCEM_PHISYM',CCEM_PHISYM,IER)
        CALL EZRSET
C
C ****  MONTE CARLO FLAG
C
        MONTE_CARLO = IQ(LHEAD+1) .GT. 999
        IF ( MONTE_CARLO ) THEN
          CALL errmsg('Monte Carlo data','CORRECTEM',
     &              'no corrections applied','w')
        ENDIF
      ENDIF
C
C
C ****  Some initialization stuff
C
      IF ( MONTE_CARLO ) THEN
C       Do nothing
        GOTO 999
      ENDIF
C
      CALL reco_version(version,pass)
      IF(version.LT.10) THEN
        CALL errmsg('reco version too old','correctem',
     &              'no corrections applied','w')
        GOTO 999
      ENDIF
C
      DE = 0.
      DO_HV_CORRS = WANT_HV_CORRS
      DO_PLSR_INSTAB_CORRS = WANT_PLSR_INSTAB_CORRS
      DO_TBMOM_CORRS = WANT_TBMOM_CORRS
      DO_PULSER_CORRS = WANT_PULSER_CORRS
      DO_CCEM_PHISYM  = WANT_CCEM_PHISYM
      DO_FIX_CSF = WANT_FIX_CSF
      DO_CLUST_CORRS = WANT_CLUST_CORRS
C
      IF(version.GE.11.AND.WANT_HV_CORRS) THEN
        CALL errmsg('RECO 11 has HV correction','correctem',
     &              'Turn off HV corr','w')
        do_hv_corrs = .false.
      ENDIF
C
      IF(version.GE.12.AND.pass.GE.10.AND.WANT_PLSR_INSTAB_CORRS) THEN
        CALL errmsg(
     &    'RECO 12.10 and higher has Pulser Instability correction',
     &    'correctem','Turn off Pulser Instability correction','w')
        do_plsr_instab_corrs = .false.
      ENDIF
C
      IF(version.GE.12.AND.pass.GE.10.AND.WANT_TBMOM_CORRS) THEN
        CALL errmsg(
     &    'RECO 12.10 and higher has Testbeam Momentum correction',
     &    'correctem','Turn off Testbeam Momentum correction','w')
        do_tbmom_corrs = .false.
      ENDIF
C
      IF(version.GE.12.AND.pass.GE.10.AND.WANT_CCEM_PHISYM) THEN
        CALL errmsg('RECO 12.10 and higher has PHI-SYMMETRY correction',
     &    'correctem','Turn off PHI-SYMMETRY corr','w')
        do_ccem_phisym = .false.
      ENDIF
C
      IF((version.EQ.12.AND.pass.LT.13.AND.WANT_CLUST_CORRS) .OR.
     &   (version.LT.12.AND.WANT_CLUST_CORRS)) THEN
        CALL errmsg('RECO 12.12 and lower have old clustering',
     &    'correctem','Turn off CLUST_CORRS corr','w')
        do_clust_corrs = .false.
      ENDIF
C
C ****  sampling fraction corrections are turned off in fix_em_sf
C ****  pulser corrections are turned off in pulser_corr
C
C
      CALL evntid(numrun,numevt)
C
      CALL VERTEX_INFO(1,NVER,VINFO,OK)
      IF (OK) THEN
        ZV = VINFO(1,1)
        DZ = VINFO(2,1)
      ELSE
        ZV = 0.0
        DZ = 0.0
        CALL ERRMSG ('CORRECTEM', 'CORRECTEM',
     &    'VERTEX_INFO returned error', 'W')
      ENDIF
C
C
      IF (PARTICLE.EQ.'E'.OR.PARTICLE.EQ.'e' ) THEN
c
        DONE = BTEST(IQ(LCLUS+31),0)
C
C
C ****  first check if we want this "electron"
C ****  to be corrected
C
        VERSION = IQ(LCLUS+1)
        IF(VERSION.EQ.1) THEN
          STATUS = IQ(LCLUS+20)
        ELSEIF (VERSION.EQ.2) THEN
          STATUS = IQ(LCLUS+20)
        ELSE
          STATUS = IQ(LCLUS+30)
        ENDIF
        IF(IAND(STATUS,ECORRECT_MASK).NE.0) THEN
          COR_FACTOR = 1.
          RETURN
        ENDIF
C
C
        IF(done) THEN
          CALL errmsg('CORRECTEM','CORRECTION ALREADY DONE',
     &      ' SKIP CORRECTIONS ','W')
        ELSE
          lcacl = lq(LCLUS-2)
          lcash = lq(lcacl-2)
          ecacl = q(lcacl+7)
          theta = q(LCLUS+8)
          phi   = q(LCLUS+10)
          CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
          EL_IETA = IETA_HOT(3)
          EL_IPHI = IPHI_HOT(3)
          IF(abs(EL_IETA).LT.13) THEN
            IF(DO_PLSR_INSTAB_CORRS) THEN
              pin_factor = pulser_instability_factor(1)
            ELSE
              pin_factor = 1.
            ENDIF
            IF(DO_TBMOM_CORRS) THEN
              tmom_factor = tb_momentum_factor(1)
            ELSE
              tmom_factor = 1.
            ENDIF
          ELSE
            IF(DO_PLSR_INSTAB_CORRS) THEN
              pin_factor = pulser_instability_factor(2)
            ELSE
              pin_factor = 1.
            ENDIF
            IF(DO_TBMOM_CORRS) THEN
              tmom_factor = tb_momentum_factor(2)
            ELSE
              tmom_factor = 1.
            ENDIF
          ENDIF
          DETA = EL_IETA/10.
C
C ****  GET CORRECTIONS...
C
          IF ( DO_CC_CRACK ) THEN
            CALL CRACK_CORR_CASH(LCLUS,ECRACK,IER)
          ELSE
            ECRACK = 0.
          ENDIF
C
          IF(DO_HV_CORRS) THEN
            CALL HV_COR_EM(NUMRUN,LCLUS,EM)
            HV_CORR_FACTOR = EM(4)/ECACL
          ELSE
            HV_CORR_FACTOR = 1.
          ENDIF
C
          IF(DO_ETA_CORRS) THEN
            ETA_FACTOR = ETA_CORR(DETA,IER)
          ELSE
            ETA_FACTOR = 1.
          ENDIF
C
          IF(DO_LARTEMP_CORRS) THEN
            IF(NUMRUN.GE.72251) THEN           !run 1b
              IZ = 2
            ELSE
              IZ = 1
            ENDIF
C
            IF(abs(EL_IETA).LT.13) THEN
              LARTEMP_FACTOR = LARTEMP_CORRS(1,IZ)
            ELSEIF(EL_IETA.LT.-12) THEN
              LARTEMP_FACTOR = LARTEMP_CORRS(2,IZ)
            ELSE
              LARTEMP_FACTOR = LARTEMP_CORRS(3,IZ)
            ENDIF
          ELSE
            LARTEMP_FACTOR = 1.
          ENDIF
          IF ( DO_LARPUR_CORRS ) THEN
            IF(abs(EL_IETA).LT.13) THEN
              LARPUR_FACTOR = LARPUR_CORRS(1)
            ELSEIF(EL_IETA.LT.-12) THEN
              LARPUR_FACTOR = LARPUR_CORRS(2)
            ELSE
              LARPUR_FACTOR = LARPUR_CORRS(3)
            ENDIF
          ELSE
            LARPUR_FACTOR = 1.
          ENDIF
C
          IF ( DO_CLUST_CORRS ) THEN
            IF(abs(EL_IETA).LT.13) THEN
              CLUST_FACTOR = CLUST_CORRS(1)
            ELSEIF(EL_IETA.LT.-12) THEN
              CLUST_FACTOR = CLUST_CORRS(2)
            ELSE
              CLUST_FACTOR = CLUST_CORRS(3)
            ENDIF
          ELSE
            CLUST_FACTOR = 1.
          ENDIF
C
          IF ( DO_CCALPHA_CORRS ) THEN
            CCALPHA_FACTOR = CCALPHA_CORR
          ELSE
            CCALPHA_FACTOR = 1.
          ENDIF
          IF ( DO_CCZSUP_CORRS ) THEN
            CCZSUP_FACTOR = CCZSUP_CORR
          ELSE
            CCZSUP_FACTOR = 1.
          ENDIF
          IF ( DO_CCCLUS_CORRS ) THEN
            CCCLUS_FACTOR = CCCLUS_CORR
          ELSE
            CCCLUS_FACTOR = 1.
          ENDIF
C
          IF ( DO_PULSER_CORRS ) THEN
            CALL PULSER_CORR(LCLUS,PULSER_CORR_FACTOR)
          ELSE
            PULSER_CORR_FACTOR = 1.
          ENDIF
C
          IF ( DO_EC_CORRS ) THEN
            CALL ECEMCR(LCLUS,ZV,DZ,DE,ERR_DE,CORR_APPLIED,IER)
          ELSE
            DE = 0
          ENDIF
C
          IF( DO_FIX_CSF ) THEN
            CALL FIX_EM_SF(LCLUS,CSF_FACTOR)
          ELSE
            CSF_FACTOR = 1.0
          ENDIF
C
          BOOST_FACTOR = 1.
          IF(DO_CCEM_BOOST.AND.ABS(EL_IETA).LT.13) BOOST_FACTOR =
     &      CCEM_BOOST
          IF(DO_ECEMN_BOOST.AND.EL_IETA.LT.-13) BOOST_FACTOR =
     &      ECEMN_BOOST
          IF(DO_ECEMS_BOOST.AND.EL_IETA.GT.13) BOOST_FACTOR =
     &      ECEMS_BOOST
C
          PHISYM_FACTOR = 1.
          IF(DO_CCEM_PHISYM.AND.ABS(EL_IETA).LT.13)
     &      PHISYM_FACTOR=CCEM_PHISYM(EL_IPHI)
C
C ****  APPLY THEM...
C
          ECLUS = (ECACL+ECRACK+DE)
     &      *HV_CORR_FACTOR*ETA_FACTOR*PULSER_CORR_FACTOR
     &      *pin_factor*tmom_factor
     &      *LARTEMP_FACTOR*LARPUR_FACTOR*CLUST_FACTOR
     &      *CCALPHA_FACTOR*CCZSUP_FACTOR*CCCLUS_FACTOR
     &      *BOOST_FACTOR*PHISYM_FACTOR*CSF_FACTOR
C
C
C ****  Compute the correction factor to return
C
          COR_FACTOR = ECLUS/Q(LCLUS+6)
C
        ENDIF
      ENDIF
C
C
C
C ****  now PPHO... Again we have to check to see if
C ****  the bank needs MZPUSHing.
C
      IF (PARTICLE.EQ.'p'.OR.PARTICLE.EQ.'P') THEN
C
        DONE = BTEST(IQ(LCLUS+31),0)
C
C
C ****  check if we want this "photon to be corrected
C
C
        VERSION = IQ(LCLUS+1)
        IF(VERSION.EQ.1) THEN
          STATUS = IQ(LCLUS+20)
        ELSEIF(VERSION.EQ.2) THEN
          STATUS = IQ(LCLUS+23)
        ELSE
          STATUS = IQ(LCLUS+30)
        ENDIF
        IF(IAND(STATUS,ECORRECT_MASK).NE.0) THEN
          COR_FACTOR = 1.
          RETURN
        ENDIF
C
        IF(done) THEN
          CALL errmsg('CORRECTEM','CORRECTION ALREADY DONE',
     &      ' SKIP CORRECTIONS ','W')
        ELSE
          lcacl = lq(LCLUS-2)
          lcash = lq(lcacl-2)
          ecacl = q(lcacl+7)
          theta = q(LCLUS+8)
          phi   = q(LCLUS+10)
          CALL CEMDPTH(LCASH,IETA_HOT,IPHI_HOT,EDPTH,PDPTH,ENERGY_HOT)
          EL_IETA = IETA_HOT(3)
          EL_IPHI = IPHI_HOT(3)
          IF(abs(EL_IETA).LT.13) THEN
            IF(DO_PLSR_INSTAB_CORRS) THEN
              pin_factor = pulser_instability_factor(1)
            ELSE
              pin_factor = 1.
            ENDIF
            IF(DO_TBMOM_CORRS) THEN
              tmom_factor = tb_momentum_factor(1)
            ELSE
              tmom_factor = 1.
            ENDIF
          ELSE
            IF(DO_PLSR_INSTAB_CORRS) THEN
              pin_factor = pulser_instability_factor(2)
            ELSE
              pin_factor = 1.
            ENDIF
            IF(DO_TBMOM_CORRS) THEN
              tmom_factor = tb_momentum_factor(2)
            ELSE
              tmom_factor = 1.
            ENDIF
          ENDIF
          DETA = EL_IETA/10.
C
C ****  GET CORRECTIONS...
C
          IF(DO_HV_CORRS) THEN
            CALL HV_COR_EM(NUMRUN,LCLUS,EM)
            HV_CORR_FACTOR = EM(4)/ECACL
          ELSE
            HV_CORR_FACTOR = 1.
          ENDIF
C
          IF(DO_ETA_CORRS) THEN
            ETA_FACTOR = ETA_CORR(DETA,IER)
          ELSE
            ETA_FACTOR = 1.
          ENDIF
C
          IF ( DO_CC_CRACK ) THEN
            CALL CRACK_CORR_CASH(LCLUS,ECRACK,IER)
          ELSE
            ECRACK = 0.
          ENDIF
C
C
          IF(DO_LARTEMP_CORRS) THEN
            IF(NUMRUN.GE.72251) THEN           !run 1b
              IZ = 2
            ELSE
              IZ = 1
            ENDIF
C
            IF(abs(EL_IETA).LT.13) THEN
              LARTEMP_FACTOR = LARTEMP_CORRS(1,IZ)
            ELSEIF(EL_IETA.LT.-12) THEN
              LARTEMP_FACTOR = LARTEMP_CORRS(2,IZ)
            ELSE
              LARTEMP_FACTOR = LARTEMP_CORRS(3,IZ)
            ENDIF
          ELSE
            LARTEMP_FACTOR = 1.
          ENDIF
          IF ( DO_LARPUR_CORRS ) THEN
            IF(abs(EL_IETA).LT.13) THEN
              LARPUR_FACTOR = LARPUR_CORRS(1)
            ELSEIF(EL_IETA.LT.-12) THEN
              LARPUR_FACTOR = LARPUR_CORRS(2)
            ELSE
              LARPUR_FACTOR = LARPUR_CORRS(3)
            ENDIF
          ELSE
            LARPUR_FACTOR = 1.
          ENDIF
C
          IF ( DO_CLUST_CORRS ) THEN
            IF(abs(EL_IETA).LT.13) THEN
              CLUST_FACTOR = CLUST_CORRS(1)
            ELSEIF(EL_IETA.LT.-12) THEN
              CLUST_FACTOR = CLUST_CORRS(2)
            ELSE
              CLUST_FACTOR = CLUST_CORRS(3)
            ENDIF
          ELSE
            CLUST_FACTOR = 1.
          ENDIF
C
          IF ( DO_CCALPHA_CORRS ) THEN
            CCALPHA_FACTOR = CCALPHA_CORR
          ELSE
            CCALPHA_FACTOR = 1.
          ENDIF
          IF ( DO_CCZSUP_CORRS ) THEN
            CCZSUP_FACTOR = CCZSUP_CORR
          ELSE
            CCZSUP_FACTOR = 1.
          ENDIF
          IF ( DO_CCCLUS_CORRS ) THEN
            CCCLUS_FACTOR = CCCLUS_CORR
          ELSE
            CCCLUS_FACTOR = 1.
          ENDIF
C
C
          IF ( DO_PULSER_CORRS ) THEN
            CALL PULSER_CORR(LCLUS,PULSER_CORR_FACTOR)
          ELSE
            PULSER_CORR_FACTOR = 1.
          ENDIF
C
          IF ( DO_EC_CORRS ) THEN
            CALL ECEMCR(LCLUS,ZV,DZ,DE,ERR_DE,CORR_APPLIED,IER)
          ELSE
            DE = 0
          ENDIF
C
          IF( DO_FIX_CSF ) THEN
            CALL FIX_EM_SF(LCLUS,CSF_FACTOR)
          ELSE
            CSF_FACTOR = 1.0
          ENDIF
C
          BOOST_FACTOR = 1.
          IF(DO_CCEM_BOOST.AND.ABS(EL_IETA).LT.13) BOOST_FACTOR =
     &      CCEM_BOOST
          IF(DO_ECEMN_BOOST.AND.EL_IETA.LT.-13) BOOST_FACTOR =
     &      ECEMN_BOOST
          IF(DO_ECEMS_BOOST.AND.EL_IETA.GT.13) BOOST_FACTOR =
     &      ECEMS_BOOST
C
          PHISYM_FACTOR = 1.
          IF(DO_CCEM_PHISYM.AND.ABS(EL_IETA).LT.13)
     &      PHISYM_FACTOR=CCEM_PHISYM(EL_IPHI)
C
C ****  APPLY THEM...
C
          ECLUS = (ECACL+ECRACK+DE)
     &      *HV_CORR_FACTOR*ETA_FACTOR*PULSER_CORR_FACTOR
     &      *pin_factor*tmom_factor
     &      *LARTEMP_FACTOR*LARPUR_FACTOR*CLUST_FACTOR
     &      *CCALPHA_FACTOR*CCZSUP_FACTOR*CCCLUS_FACTOR
     &      *BOOST_FACTOR*PHISYM_FACTOR*CSF_FACTOR
C
C ****  Compute the correction factor to return
C
          COR_FACTOR = ECLUS/Q(LCLUS+6)
          RETURN
        ENDIF
      ENDIF
C
C#######################################################################
      ENTRY CORRECTEM_BEGIN()
      CORRECTEM_BEGIN = .TRUE.
      CALL INRCP('CORRECTEM_RCP',IER)
      IF (IER.NE.0) THEN
        CALL ERRMSG('CORRECTEM','CORRECTEM_BEGIN',
     &    ' NO CORRECTEM_RCP FILE ','W')
        CORRECTEM_BEGIN = .FALSE.
      ENDIF
C
      RETURN
C
C#######################################################################
      ENTRY CORRECTEM_END()
      CORRECTEM_END = .TRUE.
      RETURN
c
  999 RETURN
      END
