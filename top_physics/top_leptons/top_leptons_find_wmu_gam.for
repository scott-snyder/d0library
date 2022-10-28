      SUBROUTINE TOP_LEPTONS_FIND_WMU_GAM(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine for WWgamma events
C-                         where W decays muonically
C-
C-   Inputs  :
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-             MISSET_FACTOR   - multiplicative correction to missing
C-                               Et value
C-             PTMIN_LEPTON    - Ptmin cut for lepton
C-             PTMIN_PHOTON    - Ptmin cut for photon
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-
C-   Outputs :
C-              IFGOOD = .TRUE./.FALSE - event is/isn't a good candidate
C-
C-   Controls: None
C-
C-   Created  29-Jan-1993   Stephen J. Wimpenny
C-   Updated  31-JAN-1993   Brajesh C  Choudhary dR(lepton,photon) and
C-                                               transverse cluster mass
C-                                               calculation for E_GAM_missET.
C-                                               JEllison's version modified.
C-   Modified  5-Feb-1993   MT_CLUSTER calculation protected against
C-                          screwy measurements
C-   Modified 16-Mar-1993   Name changes for Good_Electron, Good_Photon,
C-                          Good_Jet and Good_Muon logicals, Mass4,
C-                          Dr_From_Deta_Dphi
C-   Updated   3-MAY-1993   Brajesh C Choudhary  Fixed bug in reading Etmiss.
C-                          Major update. Include isolation on lepton & photon.
C-                          Also implement dphi and mass and their correlation  
C-                          cuts on lepton-photon pair.
C-   Modified 17-Jul-1993   Fix to equivalence bug in s/r call
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR,TOP_LEPTONS_UTIL_MASS4
C
      LOGICAL FIRST,IFGOOD
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      LOGICAL DO_ISOL_CUT_WGAM,DO_DPHI_CUT,DO_MASS_CUT,DO_MASS_DPHI_CUT
C
      INTEGER NOMU,NOEL,NOPH,NOJT,IER
      INTEGER LPELC_VEC(5),LPPHO_VEC(5),LPMUO_VEC(5),LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_MU,I_JT,LJETS_DR,LJETS_DPHI
      INTEGER GZPELC,GZPPHO,GZPMUO,GZPNUT,GZJETS
      INTEGER LPELC,LPPHO,LPMUO,LPNUT,LJETS
C
      REAL DR_MIN_LEP_JET,DR_ISOL_06_LEP,DR_MIN_PHO_JET
      REAL DPHI_MAX_LEP_PHOT,MIN_MASS_LEP_PHOT,MAX_MASS_LEP_PHOT
      REAL DR_MIN,DPHI_MIN,DPHI_LEP_PHOT,PI,TWOPI,CONV
      REAL PTMIN_LEPTON,PTMIN_PHOTON
      REAL MISSET_MIN_CORR,MISSET_CORR,MET_VEC(3)
      REAL MIN_CLUSTER_MT,DR_MIN_LEP_PHO
      REAL DPHI,DETA,DR_VEC12,TOP_LEPTONS_UTIL_CALC_DR
      REAL DILEP_VEC(4),TWO_BODY_MASS,TOP_LEPTONS_UTIL_MASS4
      REAL VEC2(4),MT_CLUSTER,VMOD
C
      DATA PI,TWOPI,CONV/3.141593,6.283185,57.2958/
      DATA FIRST/.TRUE./
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('WWG_LEPTON_PTMIN',PTMIN_LEPTON,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_PHOTON_PTMIN',PTMIN_PHOTON,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET('WWG_MIN_MT_CLUSTER',MIN_CLUSTER_MT,
     &    IER)
        IF (IER.EQ.0) CALL EZGET('WWG_DRMIN_LEP_PHO',DR_MIN_LEP_PHO,IER)
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
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_WMU_GAM',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_EL=0
      I_PH=0
      I_MU=0
      I_JT=0
C
C *** Get all relevent bank pointers
C *** PNUT3
C
      MISSET_CORR=0.
      LPNUT=GZPNUT(3)
      IF(LPNUT.NE.0) MISSET_CORR=Q(LPNUT+7)
C
      IF(NOMU.LT.1) GO TO 999
C
C *** PMUO
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.GT.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.LT.1) GO TO 999
      IF(NOEL.LT.1) GO TO 50
C
C *** PELC
C
      LPELC=GZPELC()
      DO WHILE(LPELC.GT.0)
        IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
          I_EL=I_EL+1
          IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
   50 IF(NOPH.LT.1) GO TO 100
C
C *** PPHO
C
      LPPHO=GZPPHO()
      DO WHILE(LPPHO.GT.0)
        IF(TOP_LEPTONS_GOOD_PHOTON(LPPHO)) THEN
          I_PH=I_PH+1
          IF(I_PH.LT.6) LPPHO_VEC(I_PH)=LPPHO
        ENDIF
        LPPHO=LQ(LPPHO)
      ENDDO
  100 CONTINUE
C
C *** JETS
C
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11) LJETS_VEC(I_JT)=LJETS
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.GT.10) I_JT=10
C-----------------------------------------------------------------------
C
C *** Muon-Electron (PMUO+PELC) selection
C
      IF(I_MU.LT.1) GO TO 999
      IF(I_EL.LT.1) GO TO 200
C
C *** Pt cuts
C *** i.) Ptmin lepton  and Ptmin photon
C
      IF(Q(LPMUO_VEC(1)+14).LT.PTMIN_LEPTON.OR.Q(LPELC_VEC(1)+7)
     &   .LT.PTMIN_PHOTON) GO TO 200
C
C *** Missing Et
C
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 200
C
C *** Cut on delta-R between muon and electron
C
      DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPMUO_VEC(1)+17))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DETA=(Q(LPELC_VEC(1)+9)-Q(LPMUO_VEC(1)+16))
      DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
      IF(DR_VEC12.LT.DR_MIN_LEP_PHO) GO TO 200
C
C *** Cut on cluster transverse mass of PMUO, PELC and PNUT(3)
C
        CALL UCOPY (Q(LPMUO_VEC(1)+10),DILEP_VEC(1),4)
        CALL VADD(Q(LPELC_VEC(1)+3),DILEP_VEC(1),DILEP_VEC(1),4)
        TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
        CALL VADD(DILEP_VEC,Q(LPNUT+3),VEC2,2)
        MT_CLUSTER = ( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     &      +VMOD(Q(LPNUT+3),2) )**2 - VMOD(VEC2,2)**2
        IF(MT_CLUSTER.GT.0.) THEN
          MT_CLUSTER=SQRT(MT_CLUSTER)
        ELSE
          MT_CLUSTER=0.
        ENDIF
        IF(MT_CLUSTER.LT.MIN_CLUSTER_MT) GO TO 200
C
C *** Isolation cuts on WGAMMA muon and electron
C
      IF(DO_ISOL_CUT_WGAM) THEN
C
C *** Isolation cut on muon
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(1)+16),
     1    Q(LPMUO_VEC(1)+17),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_LEP_JET) GO TO 200 
C
C *** 2.) 3 sigma isolation cut in large cone (dR=0.6)
C
        IF(Q(LPMUO_VEC(1)+32).GT.DR_ISOL_06_LEP) GO TO 200
C
C *** Isolation cut on electron 
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1    Q(LPELC_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_PHO_JET) GO TO 200
      ENDIF
C
C *** dPhi max (muon-electron)
C
      DPHI_LEP_PHOT= ABS( Q(LPMUO_VEC(1)+17)-Q(LPELC_VEC(1)+10) )
      IF(DPHI_LEP_PHOT.GT.PI) DPHI_LEP_PHOT=TWOPI-DPHI_LEP_PHOT
      DPHI_LEP_PHOT=DPHI_LEP_PHOT*CONV
      IF(DO_DPHI_CUT) THEN
        IF(DPHI_LEP_PHOT.GT.DPHI_MAX_LEP_PHOT) GO TO 200
      ENDIF
C
C *** Lepton-Photon Mass Window Cut
C
      IF(DO_MASS_CUT) THEN
        IF(TWO_BODY_MASS.GE.MIN_MASS_LEP_PHOT                           
     &     .AND.TWO_BODY_MASS.LE.MAX_MASS_LEP_PHOT)GO TO 200
      ENDIF
C
C *** Mass(lepton-photon) - DPHI(lepton-photon) Correllation
C 
      IF(DO_MASS_DPHI_CUT) THEN
        IF((DPHI_LEP_PHOT.GT.DPHI_MAX_LEP_PHOT).AND.
     1    (TWO_BODY_MASS.GE.MIN_MASS_LEP_PHOT                           
     2     .AND.TWO_BODY_MASS.LE.MAX_MASS_LEP_PHOT))GO TO 200
      ENDIF
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
  200 CONTINUE
C-----------------------------------------------------------------------
C
C *** Muon-Photon (PMUO+PPHO) selection
C
      IF(I_PH.LT.1) GO TO 999
C
C *** Apply selection cuts
C *** Pt min cuts lepton photon
C
      IF(Q(LPMUO_VEC(1)+14).LT.PTMIN_LEPTON.OR.Q(LPPHO_VEC(1)+7)
     1  .LT.PTMIN_PHOTON) GO TO 999
C
C ***  Missing Et cut
C
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
C
C *** Cut on delta-R between muon and photon
C
      DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPMUO_VEC(1)+17))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DETA=(Q(LPPHO_VEC(1)+9)-Q(LPMUO_VEC(1)+16))
      DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
      IF(DR_VEC12.LT.DR_MIN_LEP_PHO) GO TO 999
C
C *** Cut on cluster transverse mass of PMUO, PELC and PNUT(3)
C
        CALL UCOPY (Q(LPMUO_VEC(1)+10),DILEP_VEC(1),4)
        CALL VADD(Q(LPPHO_VEC(1)+3),DILEP_VEC(1),DILEP_VEC(1),4)
        TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
        CALL VADD(DILEP_VEC,Q(LPNUT+3),VEC2,2)
        MT_CLUSTER = ( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     &    +VMOD(Q(LPNUT+3),2) )**2 - VMOD(VEC2,2)**2
        IF(MT_CLUSTER.GT.0.) THEN
          MT_CLUSTER=SQRT(MT_CLUSTER)
        ELSE
          MT_CLUSTER=0.
        ENDIF
        IF(MT_CLUSTER.LT.MIN_CLUSTER_MT) GO TO 999

C
C *** Isolation cuts on WGAMMA muon and electron
C
      IF(DO_ISOL_CUT_WGAM) THEN
C
C *** Isolation cut on muon
C *** 1.) nearside jet cut

        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPMUO_VEC(1)+16),
     1    Q(LPMUO_VEC(1)+17),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_LEP_JET) GO TO 999
C
C *** 2.) 3 sigma isolation cut in large cone (dR=0.6)
C
        IF(Q(LPMUO_VEC(1)+32).GT.DR_ISOL_06_LEP) GO TO 999
C
C *** Isolation cut on photon
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(1)+9),
     1    Q(LPELC_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_PHO_JET) GO TO 999
      ENDIF
C
C *** dPhi max (muon-photon)
C
      DPHI_LEP_PHOT= ABS( Q(LPMUO_VEC(1)+17)-Q(LPPHO_VEC(1)+10) )
      IF(DPHI_LEP_PHOT.GT.PI) DPHI_LEP_PHOT=TWOPI-DPHI_LEP_PHOT
      DPHI_LEP_PHOT=DPHI_LEP_PHOT*CONV
      IF(DO_DPHI_CUT) THEN
        IF(DPHI_LEP_PHOT.GT.DPHI_MAX_LEP_PHOT) GO TO 999
      ENDIF
C
C *** Lepton-Photon Mass Window Cut
C
      IF(DO_MASS_CUT) THEN
        IF(TWO_BODY_MASS.GE.MIN_MASS_LEP_PHOT                           
     &     .AND.TWO_BODY_MASS.LE.MAX_MASS_LEP_PHOT)GO TO 999
      ENDIF
C
C *** Mass(lepton-photon) - DPHI(lepton-photon) Correllation
C 
      IF(DO_MASS_DPHI_CUT) THEN
        IF((DPHI_LEP_PHOT.GT.DPHI_MAX_LEP_PHOT).AND.
     1    (TWO_BODY_MASS.GE.MIN_MASS_LEP_PHOT                           
     2     .AND.TWO_BODY_MASS.LE.MAX_MASS_LEP_PHOT))GO TO 999
      ENDIF
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
