      SUBROUTINE TOP_LEPTONS_FIND_EE(IFGOOD,NOMU,NOEL,NOPH,
     1  NOJT,MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine to look for top->e e 
C-                                  event candidates
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
C-             PTMIN_LEP1      - Ptmin cut for lepton1
C-             PTMIN_LEP2      - Ptmin cut for lepton2
C-             NOJT_MIN        - minimum jet multiplicity (after cuts)
C-             MISSET_MIN_CALO - minimum missing Et (Calo+ICD+MG)
C-             MISSET_MIN_CORR - minimum missing Et (muon corrected)
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isnt a candidate
C-
C-   Controls: None
C-
C-   Created  19-JUL-1992   Stephen J. Wimpenny
C-   Modified 21-JUL-1992   Di-electron,Electron-Photon,Photon-Photon
C-                            code implemented (Pt cuts only)
C-   Modified 17-Sep-1992   Read cuts directly from RCP file
C-   Modified 24-Sep-1992   Uses Good Electron and Good Photon logicals
C-   Modified 28-Jan-1993   Jet multiplicity and Et cuts added
C-   Modified 16-Mar-1993   Name changes to routines Good_Electron,
C-                          Good_Photon, Good_Jet
C-   Modified 22-Apr-1993   Jet Energy corrections added
C-   Modified 17-Jul-1993   em scale corrections added
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_JET
      EXTERNAL TOP_LEPTONS_GOOD_ELECTRON
C
      LOGICAL FIRST,IFGOOD,CORR_JETS
      LOGICAL TOP_LEPTONS_GOOD_PHOTON,TOP_LEPTONS_GOOD_JET
      LOGICAL TOP_LEPTONS_GOOD_ELECTRON
C
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,IER
      INTEGER LPELC,LPPHO,LPNUT,LJETS
      INTEGER GZPELC,GZPPHO,GZPNUT,GZJETS
      INTEGER LPELC_VEC(5),LPPHO_VEC(5),LJETS_VEC(10)
      INTEGER I_EL,I_PH,I_JT,I_EL_ISOL,I_PH_ISOL
      INTEGER LPELC_VEC_ISOL(5),LPPHO_VEC_ISOL(5)
C
      REAL PTMIN_JET1,PTMIN_JET2
      REAL MISSET_MIN_CALO,MISSET_MIN_CORR
      REAL MISSET_CALO,MISSET_CORR
      REAL ET1,ET2,JET1_ET,JET2_ET
      REAL JETS_EX,JETS_EY,JETS_EZ,JETS_E,JETS_ET,JETS_PHI,JETS_ETA
      REAL MET_VEC(3)
C
      DATA FIRST/.TRUE./

C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('PNUT3_ETMIN',MISSET_MIN_CORR,IER)
        IF (IER.EQ.0) CALL EZGET('PNUT2_ETMIN',MISSET_MIN_CALO,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET('JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('JET1_PTMIN',PTMIN_JET1,IER)
        IF (IER.EQ.0) CALL EZGET('JET2_PTMIN',PTMIN_JET2,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_EE',' ','F')
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
        MISSET_CALO=(Q(LPNUT+3)+MET_VEC(1))**2 +
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
C *** Look for isolated electrons passing Et thresholds
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.2) GO TO 200
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 999
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      ENDIF
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 250
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
  250 CONTINUE
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
C *** Look for an isolated electron above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLEL(I_EL,LPELC_VEC,I_EL_ISOL,
     1  LPELC_VEC_ISOL,I_JT)
      IF(I_EL_ISOL.LT.1) GO TO 999
C
C *** and an isolated photon above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.1) GO TO 999
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 999
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      ENDIF
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 350
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
          IF(IER.GE.0) THEN
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
          IF(IER.GE.0) THEN
            JET1_ET=ET1
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1) GO TO 999
      ENDIF
  350 CONTINUE
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
C *** Look for two isolated photons above threshold
C
      CALL TOP_LEPTONS_FIND_ISOLPH(I_PH,LPPHO_VEC,I_PH_ISOL,
     1  LPPHO_VEC_ISOL,I_JT)
      IF(I_PH_ISOL.LT.2) GO TO 999
C
C *** Missing Et cut
C
      IF(NOMU.LT.1) THEN
        IF(MISSET_CALO.LT.MISSET_MIN_CALO) GO TO 999
      ELSE
        IF(MISSET_CORR.LT.MISSET_MIN_CORR) GO TO 999
      ENDIF
C
C *** Jet multiplicity and Et selection
C
      IF(NOJT_MIN.LT.1) GO TO 450
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
          IF(IER.GE.0) THEN
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
          IF(IER.GE.0) THEN
            JET1_ET=ET1
          ELSE
            JET1_ET=Q(LJETS_VEC(1)+6)
          ENDIF
        ELSE
          JET1_ET=Q(LJETS_VEC(1)+6)
        ENDIF
        IF(JET1_ET.LT.PTMIN_JET1) GO TO 999
      ENDIF
  450 CONTINUE
C
C *** Good candidate
C
      IFGOOD=.TRUE.
C----------------------------------------------------------------------
  999 RETURN
      END
