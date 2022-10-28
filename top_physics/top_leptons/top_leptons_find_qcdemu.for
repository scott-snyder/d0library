      SUBROUTINE TOP_LEPTONS_FIND_QCDEMU(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event study routine for QCD background to top
C-                         search in e-mu decay channel
C-
C-   Inputs  : 
C-             NOMU         - no of 'good' PMUO candidates
C-             NOEL         - no of 'good' PELC candidates
C-             NOPH         - no of 'good' PPHO candidates
C-             NOJT         - no of 'good' JETS candidates
C-
C-   RCP file parameters :
C-
C-             CORR_JETS    - .TRUE./.FALSE. do/dont apply jet
C-                               corrections
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-             MISSET_MAX_CORR - maximum missing Et (muon corrected)
C-             MISSET_MIN_CALO - minimum missing Et (Calorimeter+ICD)
C-             MISSET_MAX_CALO - maximum missing Et (Calorimeter+ICD)
C-             NOJT_MIN        - minmum jet multiplicity (after cuts)
C-             PTMIN_JET1      - Ptmin cut for leading jet
C-             PTMIN_JET2      - Ptmin cut for second jet
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isn't a good candidate
C-
C-   Controls: None
C-
C-   Created   1-May-1993   Stephen J. Wimpenny
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
      EXTERNAL TOP_LEPTONS_UTIL_CALC_DR
C
      LOGICAL FIRST,IFGOOD,CORR_JETS
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,IV,IER,MODE
      INTEGER LPELC_VEC(5),LPELC_VEC_ISOL(5)
      INTEGER LPPHO_VEC(5),LPPHO_VEC_ISOL(5)
      INTEGER LPMUO_VEC(5),LPMUO_VEC_NOISOL(5)
      INTEGER LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_MU,I_JT
      INTEGER I_EL_ISOL,I_PH_ISOL,I_MU_NOISOL
      INTEGER GZPELC,GZPPHO,GZPMUO,GZPNUT,GZJETS
      INTEGER LPELC,LPPHO,LPMUO,LPNUT,LJETS
C
      REAL PI,TWOPI,CONV,TOP_LEPTONS_UTIL_CALC_DR
      REAL PTMIN_JET1,PTMIN_JET2
      REAL MISSET_MIN_CORR,MISSET_MAX_CORR,MET_VEC(3),MISSET_CORR
      REAL MISSET_MIN_CALO,MISSET_MAX_CALO,MISSET_CORR2
      REAL VMOD,VADD,ET1,ET2,JET1_ET,JET2_ET
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_ET,JETS_PHI,JETS_ETA
C
      DATA PI,TWOPI,CONV/3.141593,6.283185,57.2958/
      DATA FIRST/.TRUE./
      DATA MODE/ -1/
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('QCD_PNUT2_ETMIN',MISSET_MIN_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('QCD_PNUT2_ETMAX',MISSET_MAX_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('QCD_PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET('QCD_PNUT3_ETMAX',MISSET_MAX_CORR,IER)
        IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET_i('QCD_JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('QCD_JET1_PTMIN',PTMIN_JET1,IER)
        IF (IER.EQ.0) CALL EZGET('QCD_JET2_PTMIN',PTMIN_JET2,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_QCDEMU',' ','F')
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
C *** PNUT2
C
      MISSET_CORR2=0.
      LPNUT=GZPNUT(4)
      IF(LPNUT.LE.0) LPNUT=GZPNUT(2)
      IF(LPNUT.NE.0) THEN
        MISSET_CORR2=(Q(LPNUT+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT+4)+MET_VEC(2))**2
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
      IF(I_MU.LT.1) GO TO 999
C
C *** Look for muon-in-jet
C
      CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_NOISOL,
     1  LPMUO_VEC_NOISOL,I_JT,MODE)
      IF(I_MU_NOISOL.LT.1) GO TO 999
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
C *** PNUT2 cuts
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 200
      IF(MISSET_CORR2.GT.MISSET_MAX_CALO) GO TO 200
C
C *** PNUT3 cuts
C
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 200
      IF(MISSET_CORR.GT.MISSET_MAX_CORR) GO TO 200
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 210
      IF(I_JT.LT.NOJT_MIN) GO TO 200
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET2,
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
C *** PNUT2 cuts
C
      IF(MISSET_CORR2.LT.MISSET_MIN_CALO) GO TO 999
      IF(MISSET_CORR2.GT.MISSET_MAX_CALO) GO TO 999
C
C *** PNUT3 cuts
C
      IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      IF(MISSET_CORR.GT.MISSET_MAX_CORR) GO TO 999
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 300
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      IF(I_JT.GT.1) THEN
C
C *** Apply Jet corrections if requested
C
        IF(CORR_JETS) THEN
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET1,
     1      JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
          CALL TOP_LEPTONS_CORR_JETPARM(LJETS_VEC(1),JETS_E,ET2,
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
