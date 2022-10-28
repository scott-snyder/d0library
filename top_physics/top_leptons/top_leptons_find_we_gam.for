      SUBROUTINE TOP_LEPTONS_FIND_WE_GAM(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for WWgamma 
C-                         event candidates with a W->enu decay
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
C-             CORR_JETS    - .TRUE./.FALSE. do/dont apply jet
C-                               corrections
C-             PTMIN_LEP       - Ptmin cut for lepton
C-             PTMIN_PHOT      - Ptmin cut for photon
C-             MISSET_MIN_CALO - minimum missing Et (Calo+ICD+MG)
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isnt a candidate
C-
C-   Controls: None
C-
C-   Created  29-Jan-1993   Stephen J. Wimpenny
C-   Updated  31-JAN-1993   Brajesh C Choudhary  dR(lepton,photon) and
C-                                               transverse cluster mass 
C-                                               calculation for E_GAM_missET.
C-                                               Corrected JEllison's version.
C-   Modified  5-Feb-1993   MT_CLUSTER calculation protected against
C-                          screwy parameters
C-   Modified 15-Mar-1993   Names of Good_Electron, Good_Photon and
C-                          Good_Jet logicals changed, Mass4,
C-                          Dr_From_Deta_Dphi
C-   Updated   3-MAY-1993   Brajesh C Choudhary  Major update. Include          
C-                          isolation on lepton & photon. Also implement dphi   
C-                          and mass and their correlation cuts on              
C-                          lepton-photon pair.
C-   Modified 17-Jul-1993   Fix to remove equivalenced s/r call 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_JET,TOP_LEPTONS_GOOD_PHOTON
      EXTERNAL TOP_LEPTONS_GOOD_ELECTRON,TOP_LEPTONS_UTIL_MASS4
C
      LOGICAL FIRST,IFGOOD
      LOGICAL TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      LOGICAL DO_ISOL_CUT_WGAM,DO_DPHI_CUT,DO_MASS_CUT,DO_MASS_DPHI_CUT
C
      INTEGER NOMU,NOEL,NOPH,NOJT,IER
      INTEGER LPELC,LPPHO,LPNUT,LJETS,LJETS_DR,LJETS_DPHI
      INTEGER GZPELC,GZPPHO,GZPNUT,GZJETS
      INTEGER LPELC_VEC(5),LPPHO_VEC(5),LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_JT
C
      REAL DR_MIN_LEP_JET,DR_ISOL_06_LEP,DR_MIN_PHO_JET
      REAL DPHI_MAX_LEP_PHOT,MIN_MASS_LEP_PHOT,MAX_MASS_LEP_PHOT
      REAL DR_MIN,DPHI_MIN,DPHI_LEP_PHOT
      REAL PTMIN_LEPTON,PTMIN_PHOTON
      REAL MIN_CLUSTER_MT,DR_MIN_LEP_PHO
      REAL MISSET_MIN_CALO,MISSET_MIN_CORR
      REAL MISSET_CALO,MISSET_CORR,MET_VEC(3)
      REAL TOP_LEPTONS_UTIL_CALC_DR
      REAL DILEP_VEC(4),TWO_BODY_MASS,TOP_LEPTONS_UTIL_MASS4
      REAL VEC2(4),MT_CLUSTER,VMOD
      REAL PI,TWOPI,CONV,DPHI,DETA,DR_VEC12
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
        IF (IER.EQ.0) CALL EZGET('WWG_PNUT2_ETMIN',MISSET_MIN_CALO,IER)
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
     &    'TOP_LEPTONS_FIND_WE_GAM',' ','F')
        FIRST=.FALSE.
      ENDIF
C
      IFGOOD=.FALSE.
      I_EL=0
      I_PH=0
      I_JT=0
C
C *** Get Missing Et info
C
      MISSET_CALO=0.
      LPNUT=GZPNUT(4)
      IF(LPNUT.LE.0) LPNUT=GZPNUT(2)
      IF(LPNUT.NE.0) THEN
        MISSET_CALO= (Q(LPNUT+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT+4)+MET_VEC(2))**2
        MISSET_CALO=SQRT(MISSET_CALO)
      ENDIF
      IF(NOMU.GT.0) THEN
        MISSET_CORR=0.
        LPNUT=GZPNUT(3)
        IF(LPNUT.NE.0) MISSET_CORR=Q(LPNUT+7)
      ENDIF
C
C *** Get PELC Bank pointers
C
      IF(NOEL.LT.1) GO TO 50
C
      IF(NOEL.GT.5) CALL ERRMSG('Electron Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_EE',' ','W')
C
C *** Loop over banks and extract pointers
C
      LPELC=GZPELC()
      DO WHILE(LPELC.NE.0)
        IF(TOP_LEPTONS_GOOD_ELECTRON(LPELC)) THEN
          I_EL=I_EL+1
          IF(I_EL.LT.6) LPELC_VEC(I_EL)=LPELC
        ENDIF
        LPELC=LQ(LPELC)
      ENDDO
C
C *** Get PPHO Bank pointers
C
   50 IF(NOPH.LT.1) GO TO 100
C
      IF(NOPH.GT.5) CALL ERRMSG('Photon Store Truncated at 5',
     1 'TOP_LEPTONS_FIND_EE',' ','W')
C
C *** Loop over banks and extract pointers
C
      LPPHO=GZPPHO()
      DO WHILE(LPPHO.NE.0)
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
C *** di-electron selection -> require at least 2 'good' PELC banks
C
      IF(I_EL.LT.2) GO TO 200
C
C *** Apply Pt cuts on electron 1 and electron 2
C
      IF(Q(LPELC_VEC(1)+7).LT.PTMIN_LEPTON.OR.Q(LPELC_VEC(2)+7).LT.
     &  PTMIN_PHOTON) GO TO 200
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 200 
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 200
      ENDIF
C
C *** Cut on delta-R between electron 1 and electron 2
C
      DPHI=ABS(Q(LPELC_VEC(1)+10)-Q(LPELC_VEC(2)+10))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DETA=(Q(LPELC_VEC(1)+9)-Q(LPELC_VEC(2)+9))
      DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
      IF(DR_VEC12.LT.DR_MIN_LEP_PHO) GO TO 200
C
C *** Cut on cluster transverse mass of PELC 1, PELC 2 and PNUT(2)
C
      CALL UCOPY (Q(LPELC_VEC(1)+3),DILEP_VEC(1),4)
      CALL VADD(Q(LPELC_VEC(2)+3),DILEP_VEC(1),DILEP_VEC(1),4)
      TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
      CALL VADD(DILEP_VEC,Q(LPNUT+2),VEC2,2)
      MT_CLUSTER = ( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     &      +VMOD(Q(LPNUT+2),2) )**2 - VMOD(VEC2,2)**2 
      IF(MT_CLUSTER.GT.0.) THEN
        MT_CLUSTER=SQRT(MT_CLUSTER)
      ELSE
        MT_CLUSTER=0.
      ENDIF
      IF(MT_CLUSTER.LT.MIN_CLUSTER_MT) GO TO 200
C
C *** Isolation cuts on WGAMMA electron 1 and electron 2
C
      IF(DO_ISOL_CUT_WGAM) THEN
C
C *** Isolation cut on electron 1
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1    Q(LPELC_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_LEP_JET) GO TO 200
C
C *** Isolation cut on electron 2
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(2)+9),
     1    Q(LPELC_VEC(2)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_PHO_JET) GO TO 200
      ENDIF
C
C *** dPhi max (electron 1 -electron 2)
C
      DPHI_LEP_PHOT= ABS( Q(LPELC_VEC(1)+10)-Q(LPELC_VEC(2)+10) )
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
C
  200 CONTINUE
C----------------------------------------------------------------------
C
C *** electron+photon selection -> require at least 1 'good' PELC
C *** bank + 1 'good' PPHO bank
C
      IF(I_EL.LT.1) GO TO 300
      IF(I_PH.LT.1) GO TO 999
C
C *** Apply Pt cuts on photon 1 and photon 2
C
      IF(Q(LPELC_VEC(1)+7).LT.PTMIN_LEPTON.OR.Q(LPPHO_VEC(1)+7).LT.
     &  PTMIN_PHOTON) GO TO 300
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 300
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 300
      ENDIF
C
C *** Cut on delta-R between electron and photon
C
      DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPELC_VEC(1)+10))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DETA=(Q(LPPHO_VEC(1)+9)-Q(LPELC_VEC(1)+9))
      DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
      IF(DR_VEC12.LT.DR_MIN_LEP_PHO) GO TO 300 
C
C *** Cut on cluster transverse mass of PELC, PPHO and PNUT(2)
C
      CALL UCOPY (Q(LPELC_VEC(1)+3),DILEP_VEC(1),4)
      CALL VADD(Q(LPPHO_VEC(1)+3),DILEP_VEC(1),DILEP_VEC(1),4)
      TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
      CALL VADD(DILEP_VEC,Q(LPNUT+2),VEC2,2)
      MT_CLUSTER = ( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     &    +VMOD(Q(LPNUT+2),2) )**2 - VMOD(VEC2,2)**2 
      IF(MT_CLUSTER.GT.0.) THEN
        MT_CLUSTER=SQRT(MT_CLUSTER)
      ELSE
        MT_CLUSTER=0.
      ENDIF
      IF(MT_CLUSTER.LT.MIN_CLUSTER_MT) GO TO 300
C
C *** Isolation cuts on WGAMMA electron  and photon 
C
      IF(DO_ISOL_CUT_WGAM) THEN
C
C *** Isolation cut on electron 
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPELC_VEC(1)+9),
     1    Q(LPELC_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_LEP_JET) GO TO 300
C
C *** Isolation cut on photon
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(1)+9),
     1    Q(LPPHO_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_PHO_JET) GO TO 300
      ENDIF
C
C *** dPhi max (electron - photon)
C
      DPHI_LEP_PHOT= ABS( Q(LPELC_VEC(1)+10)-Q(LPPHO_VEC(1)+10) )
      IF(DPHI_LEP_PHOT.GT.PI) DPHI_LEP_PHOT=TWOPI-DPHI_LEP_PHOT
      DPHI_LEP_PHOT=DPHI_LEP_PHOT*CONV
      IF(DO_DPHI_CUT) THEN
        IF(DPHI_LEP_PHOT.GT.DPHI_MAX_LEP_PHOT) GO TO 300
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
C
  300 CONTINUE
C-----------------------------------------------------------------------
C
C *** di-photon selection
C
      IF(I_PH.LT.2) GO TO 999
C
C *** Apply Pt cuts on photon 1 and photon 2
C
      IF(Q(LPPHO_VEC(1)+7).LT.PTMIN_LEPTON.OR.Q(LPPHO_VEC(2)+7).LT.
     &  PTMIN_PHOTON) GO TO 999
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 999
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      ENDIF
C
C *** Cut on delta-R between photon 1 and photon 2
C
      DPHI=ABS(Q(LPPHO_VEC(1)+10)-Q(LPPHO_VEC(2)+10))
      IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
      DETA=(Q(LPPHO_VEC(1)+9)-Q(LPPHO_VEC(2)+9))
      DR_VEC12=TOP_LEPTONS_UTIL_CALC_DR(DETA,DPHI)
      IF(DR_VEC12.LT.DR_MIN_LEP_PHO) GO TO 999
C
C *** Cut on cluster transverse mass of PPHO 1, PPHO 2 and PNUT(2)
C
      CALL UCOPY (Q(LPPHO_VEC(1)+3),DILEP_VEC(1),4)
      CALL VADD(Q(LPPHO_VEC(2)+3),DILEP_VEC(1),DILEP_VEC(1),4)
      TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
      CALL VADD(DILEP_VEC,Q(LPNUT+2),VEC2,2)
      MT_CLUSTER = ( SQRT(TWO_BODY_MASS**2+VMOD(DILEP_VEC,2)**2)
     &    +VMOD(Q(LPNUT+2),2) )**2 - VMOD(VEC2,2)**2 
      IF(MT_CLUSTER.GT.0.) THEN
        MT_CLUSTER=SQRT(MT_CLUSTER)
      ELSE
        MT_CLUSTER=0.
      ENDIF
      IF(MT_CLUSTER.LT.MIN_CLUSTER_MT) GO TO 999
C
C *** Isolation cuts on WGAMMA photon 1 and photon 2
C
      IF(DO_ISOL_CUT_WGAM) THEN
C
C *** Isolation cut on photon 1
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(1)+9),
     1    Q(LPPHO_VEC(1)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_LEP_JET) GO TO 999
C
C *** Isolation cut on PHOTON 2
C *** 1.) nearside jet cut
C
        CALL TOP_LEPTONS_UTIL_NEARJET(Q(LPPHO_VEC(2)+9),
     1    Q(LPPHO_VEC(2)+10),DR_MIN,LJETS_DR,DPHI_MIN,LJETS_DPHI)
        IF(DR_MIN.LT.DR_MIN_PHO_JET) GO TO 999
      ENDIF
C
C *** dPhi max (photon 1 -photon 2)
C
      DPHI_LEP_PHOT= ABS( Q(LPPHO_VEC(1)+10)-Q(LPPHO_VEC(2)+10) )
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
C----------------------------------------------------------------------
  999 RETURN
      END
