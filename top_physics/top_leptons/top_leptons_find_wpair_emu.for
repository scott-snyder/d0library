      SUBROUTINE TOP_LEPTONS_FIND_WPAIR_EMU(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine for WPAIR->e mu decays
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
C-             DPHI_MAX_LEP12  - max allowed dphi(lepton1-lepton2) in deg
C-             MASS12_MIN      - min allowed dilepton mass
C-             MASS12_MAX      - max allowed dilepton mass
C-             NOJT_MIN        - minmum jet multiplicity (after cuts)
C-             NOJT_MAX        - max number above ptmax (after)
C-             PTMIN_JET1      - Ptmin cut for leading jet
C-             PTMIN_JET2      - Ptmin cut for second jet
C-             PTMAX_JET       - Pt max for jet
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-             MISSET_MIN_CALO - minimum missing Et (Calorimeter+ICD)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isn't a good candidate
C-
C-   Controls: None
C-
C-   Created  19-JUL-1992   Stephen J. Wimpenny
C-   Modified 21-JUL-1992   Muon-Electron,Muon-Photon selection code
C-                                implemented (Pt cuts only)
C-   Modified 17-Sep-1992   Read cuts directly from RCP file
C-   Modified 24-Sep-1992   Used Electron/Muon/Photon Logicals 
C-   Modified 14-Dec-1992   Cuts on Min dr(e-jet,mu-jet,ph-jet) and
C-                          max dPhi(e-mu,ph-mu) added
C-   Modified 30-Dec-1992   Dilepton Mass cuts added
C-   Modified 28-Jan-1993   Jet Multiplicity Cuts Added
C-   Modified 16-Mar-1993   Name changes to Good_Electron, Good_Photon
C-                          Good_Jet and Good_Muon Logicals, Mass4,
C-                          Nearjet, Dr_From_Deta_Dphi
C-   Modified 30-Mar-1993   Mt window cut added to remove winde angle
C-                          bremm events
C-   Modified 22-Apr-1993   Jet Energy corrections added
C-   Modified 27-Apr-1993   PNUT2 cut added
C-   Modified  2-May-1993   Isolation cut logic cleaned up + collinear
C-                          brem cut moved into muon isolation code
C-   Modified 11-May-1993   (HTD) WPair version created, JET conditions.
C-   Modified 17-Jul-1993   Logic tidied up
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      EXTERNAL TOP_LEPTONS_UTIL_MASS4,TOP_LEPTONS_UTIL_CALC_DR
C
      LOGICAL FIRST,IFGOOD,CORR_JETS,corr_em
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      LOGICAL DO_MASS12_CUT,DO_DPHI12_CUT,DO_MASS_DPHI_CUT
      LOGICAL DO_WAB_CUT
C
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,IV,IER,MODE,NOJT_MAX,I
      INTEGER LPELC_VEC(5),LPELC_VEC_ISOL(5)
      INTEGER LPPHO_VEC(5),LPPHO_VEC_ISOL(5)
      INTEGER LPMUO_VEC(5),LPMUO_VEC_ISOL(5)
      INTEGER LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_MU,I_JT,I_BREM,I_BREM2,ID_CLUS
      INTEGER I_EL_ISOL,I_PH_ISOL,I_MU_ISOL
      INTEGER GZPELC,GZPPHO,GZPMUO,GZPNUT,GZJETS
      INTEGER LPELC,LPPHO,LPMUO,LPNUT2,LPNUT3,LJETS
C
      REAL PI,TWOPI,CONV,TOP_LEPTONS_UTIL_CALC_DR
      REAL PTMIN_JET1,PTMIN_JET2
      REAL MISSET_MIN_CORR,MISSET_CORR
      REAL MISSET_MIN_CALO,MISSET_CORR2,MET_VEC(3)
      REAL DPHI_LEP12,DPHI_MAX_LEP12,DILEP_VEC(4),VEC2(4)
      REAL TOP_LEPTONS_UTIL_MASS4,TWO_BODY_MASS,DETA_LEP12,DR_LEP12
      REAL MASS12_MIN,MASS12_MAX,TOP_LEPTONS_EM_CORRECTION
      REAL MT_MIN_WAB,MT_MAX_WAB,MT_CLUSTER
      REAL VMOD,VADD,ET1,ET2,JET1_ET,JET2_ET
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_ET,JETS_PHI,JETS_ETA
      REAL PTMAX_JET,JETCOUNT,TEMP
C
      DATA PI,TWOPI,CONV/3.141593,6.283185,57.2958/
      DATA FIRST/.TRUE./
      DATA MODE/ 1/
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('DO_DPHI12_CUT',DO_DPHI12_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('MAX_DPHI_L12',DPHI_MAX_LEP12,IER)
        IF (IER.EQ.0) CALL EZGET('DO_MASS12_CUT',DO_MASS12_CUT,IER)
        IF (IER.EQ.0) CALL EZGET('MASS_L12_MIN',MASS12_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('MASS_L12_MAX',MASS12_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('DO_MASS_DPHI_CUT',DO_MASS_DPHI_CUT,
     1    IER)
        IF (IER.EQ.0) CALL EZGET('I_BREM_FLAG',I_BREM,IER)
        IF (IER.EQ.0) CALL EZGET('I_BREM_FLAG2',I_BREM2,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT2_ETMIN',MISSET_MIN_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET('EM_CORR',CORR_EM,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET('JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('JET1_PTMIN',PTMIN_JET1,IER)
        IF (IER.EQ.0) CALL EZGET('JET2_PTMIN',PTMIN_JET2,IER)
        IF (IER.EQ.0) CALL EZGET('WP_JET_MULT_MAX',NOJT_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('WP_JET_PTMAX',PTMAX_JET,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_EMU',' ','F')
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
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) MISSET_CORR=Q(LPNUT3+7)
C
C *** PNUT2
C
      MISSET_CORR2=0.
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        MISSET_CORR2= (Q(LPNUT2+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT2+4)+MET_VEC(2))**2
        MISSET_CORR2=SQRT(MISSET_CORR2)
      ENDIF
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
C
      IF(I_MU.LT.1) GO TO 999
C
C *** Look for an isolated muon
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_ISOL,
     1  LPMUO_VEC_ISOL,I_JT,MODE)
      IF(I_MU_ISOL.LT.1) GO TO 999
C-----------------------------------------------------------------------
C
C *** Muon-Electron (PMUO+PELC) selection
C
      IF(I_EL.LT.1) GO TO 200
C
C *** Look for an isolated electron
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.1) GO TO 200
C
C *** Look for collinaer muon bremmstrahlung
C
      ID_CLUS=1
      CALL TOP_LEPTONS_FIND_MUBREM(I_MU_ISOL,LPMUO_VEC_ISOL,I_EL_ISOL,
     1 LPELC_VEC_ISOL,I_EL,LPELC_VEC,ID_CLUS,I_BREM)
      IF(I_EL.LT.1) GO TO 200
C
C *** Look for W->munu + wide angle bremstrahlung
C
      CALL TOP_LEPTONS_FIND_MUWAB(I_MU_ISOL,LPMUO_VEC_ISOL,I_EL,
     1 LPELC_VEC,ID_CLUS,LPNUT3,I_EL_ISOL,LPELC_VEC_ISOL,I_BREM2)
      IF(I_EL_ISOL.LT.1) GO TO 200
C
C *** dPhi max (muon-electron)
C
      DPHI_LEP12=ABS( Q(LPMUO_VEC_ISOL(1)+17)-Q(LPELC_VEC_ISOL(1)+10) )
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      IF(DO_DPHI12_CUT) THEN
        IF(DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 200
      ENDIF
C
C *** Dilepton Mass Window Cut
C
      CALL VZERO(DILEP_VEC,4)
      CALL UCOPY (Q(LPMUO_VEC_ISOL(1)+10),DILEP_VEC(1),4)
      CALL UCOPY (Q(LPELC_VEC_ISOL(1)+3),VEC2(1),4)
      IF(CORR_EM) THEN
        TEMP=TOP_LEPTONS_EM_CORRECTION(LPELC_VEC_ISOL(1))
        CALL VSCALE(VEC2(1),TEMP,VEC2(1),4)
      ENDIF
      CALL VADD(VEC2(1),DILEP_VEC(1),DILEP_VEC(1),4)
      TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
      IF(DO_MASS12_CUT) THEN
        IF(TWO_BODY_MASS.GE.MASS12_MIN.AND.TWO_BODY_MASS.LE.MASS12_MAX)
     1     GO TO 200
      ENDIF
C
C *** Mass12-dPhi12 Correllation
C
      IF(DO_MASS_DPHI_CUT) THEN
        IF((DPHI_LEP12.GT.DPHI_MAX_LEP12).AND.
     1    (TWO_BODY_MASS.GE.MASS12_MIN.AND.TWO_BODY_MASS.LE.MASS12_MAX))
     2    GO TO 200
      ENDIF
C
C *** missing Et cuts
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 200
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 200
C
C *** Jet multiplicity and Et selection
C 
      JETCOUNT = 0.
      DO I = 1,I_JT
        IF(Q(LJETS_VEC(I)+6).GT.PTMAX_JET) JETCOUNT = JETCOUNT + 1.
      ENDDO
      IF(JETCOUNT.GT.NOJT_MAX) GO TO 200

      IF(NOJT_MIN.LT.1) GO TO 210
      IF(I_JT.LT.NOJT_MIN) GO TO 200
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(2),JETS_E,ET2,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
            JET2_ET=ET2
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
            JET2_ET=Q(LJETS_VEC(2)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
          JET2_ET=Q(LJETS_VEC(2)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1.OR.JET2_ET
     1    .LT.PTMIN_JET2) GO TO 200
      ELSEIF(I_JT.GT.0) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1) GO TO 200
      ENDIF
  210 CONTINUE
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
C *** Look for an isolated photon
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** Look for collinear muon bremmstrahlung
C
      ID_CLUS=2
      CALL TOP_LEPTONS_FIND_MUBREM(I_MU_ISOL,LPMUO_VEC_ISOL,I_PH_ISOL,
     1 LPPHO_VEC_ISOL,I_PH,LPPHO_VEC,ID_CLUS,I_BREM)
      IF(I_PH.LT.1) GO TO 999
C
C *** Look for W->munu + wide angle bremstrahlung
C
      CALL TOP_LEPTONS_FIND_MUWAB(I_MU_ISOL,LPMUO_VEC_ISOL,I_PH,
     1 LPPHO_VEC,ID_CLUS,LPNUT3,I_PH_ISOL,LPPHO_VEC_ISOL,I_BREM2)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** dPhi max (muon-photon)
C
      DPHI_LEP12=ABS( Q(LPMUO_VEC_ISOL(1)+17)-Q(LPPHO_VEC_ISOL(1)+10) )
      IF(DPHI_LEP12.GT.PI) DPHI_LEP12=TWOPI-DPHI_LEP12
      DPHI_LEP12=DPHI_LEP12*CONV
      IF(DO_DPHI12_CUT) THEN
        IF(DPHI_LEP12.GT.DPHI_MAX_LEP12) GO TO 999
      ENDIF
C
C *** Dilepton Mass Window
C
      CALL VZERO(DILEP_VEC,4)
      CALL UCOPY (Q(LPMUO_VEC_ISOL(1)+10),DILEP_VEC(1),4)
      CALL VADD(Q(LPPHO_VEC_ISOL(1)+3),DILEP_VEC(1),DILEP_VEC(1),4)
      TWO_BODY_MASS=TOP_LEPTONS_UTIL_MASS4(DILEP_VEC)
      IF(DO_MASS12_CUT) THEN
        IF(TWO_BODY_MASS.GE.MASS12_MIN.AND.TWO_BODY_MASS.LE.MASS12_MAX)
     1     GO TO 999
      ENDIF
C
C *** Mass12-dPhi12 Correllation
C
      IF(DO_MASS_DPHI_CUT) THEN
        IF((DPHI_LEP12.GT.DPHI_MAX_LEP12).AND.
     1    (TWO_BODY_MASS.GE.MASS12_MIN.AND.TWO_BODY_MASS.LE.MASS12_MAX))
     2    GO TO 999
      ENDIF
C
C *** Missing Et cuts
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 999
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
C
C *** Jet multiplicity and Et selection
C
      JETCOUNT = 0.
      DO I = 1,I_JT
        IF(Q(LJETS_VEC(I)+6).GT.PTMAX_JET) JETCOUNT = JETCOUNT + 1.
      ENDDO
      IF(JETCOUNT.GT.NOJT_MAX) GO TO 999

      IF(NOJT_MIN.LT.1) GO TO 300
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(2),JETS_E,ET2,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
            JET2_ET=ET2
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
            JET2_ET=Q(LJETS_VEC(2)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
          JET2_ET=Q(LJETS_VEC(2)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1.OR.JET2_ET
     1    .LT.PTMIN_JET2) GO TO 999
      ELSEIF(I_JT.GT.0) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          IF(IER.GT.0) THEN
            JET1_ET=ET1
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1) GO TO 999
      ENDIF
  300 CONTINUE
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
