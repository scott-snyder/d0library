      SUBROUTINE TOP_LEPTONS_FIND_MUJET(IFGOOD,NOMU,NOEL,NOPH,NOJT,
     1  MET_VEC)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : event search routine for top->mu+jets decays
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
C-             PTMIN_MUON      - Ptmin cut for muon
C-             NOJT_MIN        - minmum jet multiplicity (after cuts)
C-             PTMIN_JET1      - Ptmin cut for leading jet
C-             PTMIN_JET2      - Ptmin cut for second jet
C-             PTMIN_JET3      - Ptmin cut for third jet
C-             PTMIN_JET4      - Ptmin cut for fourth jet
C-             PNUT3_ETMIN - minimum missing Et (muon corrected)
C-             DO_ISOL_CUT     -.TRUE./.FALSE. do/don't apply iso cuts
C-
C-   Outputs : 
C-              IFGOOD = .TRUE./.FALSE - event is/isn't a good candidate
C-
C-   Controls: None
C-
C-   Created  28-OCT-1992   Joey Thompson
C-   Modified 28-Jan-1993   Changes for new RCP format
C-   Modified 16-Mar-1993   Code cleanup - electron/photon index loops
C-                          removed, Good_Jet, Good_Muon logicals changed
C-   Modified 19-Mar-1993   Routine name changes for Nearjet
C-   Modified 22-Apr-1993   Jet Energy corrections added
C-   Modified  4-Dec-1993   Major re-write for background and data studies
C-                          New RCP Parameters added (JT).
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
C
      EXTERNAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
C
      LOGICAL FIRST,IFGOOD,CORR_JETS,DO_ISOL_CUT
      LOGICAL TOP_LEPTONS_GOOD_MUON,TOP_LEPTONS_GOOD_JET
      LOGICAL DO_SHAPE_JET_DROP
C
      INTEGER NOMU,NOEL,NOPH,NOJT,NOJT_MIN,IER,I
      INTEGER NOMUON_MIN,NOMUON_MAX,NOMU_ISO_MIN,NOMU_ISO_MAX
      INTEGER NOMU_NONISO_MIN,NOMU_NONISO_MAX
      INTEGER NUM_GOOD_PRI_MUON,NUM_GOOD_SEC_MUON
C
      INTEGER LPMUO_VEC(5),LPMUO_VEC_ISOL(5),LPMUO_VEC_NONISOL(5)
      INTEGER LJETS_VEC(10)
      INTEGER I_MU,I_JT
      INTEGER I_MU_ISOL,I_MU_NONISOL
      INTEGER GZPMUO,GZPNUT,GZJETS
      INTEGER LPMUO,LPNUT3,LPNUT2,LJETS
      INTEGER ISO_MODE,NONISO_MODE
      INTEGER MODE,NWAY,NSORT
      INTEGER JET_PT_INDEX(10),NOJT_MAX
      INTEGER IERR
C
      REAL MUON1_PTMIN,MUON2_PTMIN
      REAL JET_PTMIN(6),JET_PT(10),JET_ETA(10),PTMAX_NEXTJET
      REAL HT,HT_MIN,HT_JETPT_MIN,HT_JETETA_MAX,PTJTS_MIN,ETAJTS_MAX
      REAL PNUT3_ETMIN,PNUT3_ET
      REAL PNUT2_ETMIN,PNUT2_ET,MET_VEC(3)
      REAL APLAN_MIN
      REAL PI,TWOPI
      REAL JETS_E,JETS_ET,JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA
      REAL MU_PHI,MU_NEG_PHI,JET_PHI,B2B_DPHIMIN,B2B_DPHIMAX,DPHI
      REAL SPHER,PLAN,GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH
C
      DATA FIRST/.TRUE./
      DATA PI,TWOPI/3.1415927,6.2831853/
      DATA ISO_MODE,NONISO_MODE/1,-1/
C----------------------------------------------------------------------
C
C *** Read cut parameters from RCP file
C
      IER=0
      IF(FIRST) THEN
        CALL EZPICK('TOP_LEPTONS_RCP')
C
        CALL EZGET('LJ_MUON1_PTMIN',MUON1_PTMIN,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMUON_MIN',NOMUON_MIN,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMUON_MAX',NOMUON_MAX,IER)
        IF (IER.EQ.0) CALL EZGET_l('LJ_DO_ISOL_CUT',DO_ISOL_CUT,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMU_ISO_MIN',NOMU_ISO_MIN,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMU_ISO_MAX',NOMU_ISO_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_MUON2_PTMIN',MUON2_PTMIN,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMU_NONISO_MIN',NOMU_NONISO_MIN,
     1      IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_NOMU_NONISO_MAX',NOMU_NONISO_MAX,
     &       IER)
        IF (IER.EQ.0) CALL EZGET('LJ_B2B_DPHIMIN',B2B_DPHIMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_B2B_DPHIMAX',B2B_DPHIMAX,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_PNUT2_ETMIN',PNUT2_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_PNUT3_ETMIN',PNUT3_ETMIN,IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_JET_MULT_MIN',NOJT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET1_PTMIN',JET_PTMIN(1),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET2_PTMIN',JET_PTMIN(2),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET3_PTMIN',JET_PTMIN(3),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET4_PTMIN',JET_PTMIN(4),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET5_PTMIN',JET_PTMIN(5),IER)
        IF (IER.EQ.0) CALL EZGET('LJ_JET6_PTMIN',JET_PTMIN(6),IER)
        IF (IER.EQ.0) CALL EZGET_i('LJ_JET_MULT_MAX',NOJT_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_PTMAX_NEXTJET',PTMAX_NEXTJET,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_APLAN_MIN',APLAN_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_MIN',HT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETPT_MIN',HT_JETPT_MIN,IER)
        IF (IER.EQ.0) CALL EZGET('LJ_HT_JETETA_MAX',HT_JETETA_MAX,IER)
        IF (IER.EQ.0) CALL EZGET_l('JETS_CORR',CORR_JETS,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_ETAMAX',ETAJTS_MAX,IER)
        IF (IER.EQ.0) CALL EZGET('JETS_PTMIN',PTJTS_MIN,IER)
        IF (IER.EQ.0) CALL EZGET_l('LJ_DO_SHAPE_JET_DROP',
     &      DO_SHAPE_JET_DROP,IER)
        CALL EZRSET
        IF (IER.NE.0) CALL ERRMSG('Error getting RCP parameters',
     &    'TOP_LEPTONS_FIND_EMU',' ','F')
        IF ((NOEL+NOPH).GE.10)FIRST=.FALSE.   !Stop compiler complaint
        FIRST=.FALSE.
C
C *** Check consistency of HT selection and Jet preselection
C *** - reset limits if necessary
C
        IF(HT_JETPT_MIN.LT.PTJTS_MIN) HT_JETPT_MIN=PTJTS_MIN
        IF(HT_JETETA_MAX.GT.ETAJTS_MAX) HT_JETETA_MAX=ETAJTS_MAX
      ENDIF
C
C *** Set up and exit immediately if basic cuts aren't met
C
      IFGOOD=.FALSE.
      IF(NOMU.LT.NOMUON_MIN) GO TO 999
      IF(NOJT.LT.NOJT_MIN) GO TO 999
      I_MU=0
      I_JT=0
      HT = 0.
      GAPLAN = 0.
      NUM_GOOD_PRI_MUON = 0
      NUM_GOOD_SEC_MUON = 0
C
C *** Get all relevent bank pointers
C *** PNUT3
C
      PNUT3_ET=0.
      LPNUT3=GZPNUT(3)
      IF(LPNUT3.NE.0) PNUT3_ET=Q(LPNUT3+7)
C
C *** PNUT2
C
      PNUT2_ET=0.
      LPNUT2=GZPNUT(4)
      IF(LPNUT2.LE.0) LPNUT2=GZPNUT(2)
      IF(LPNUT2.NE.0) THEN
        PNUT2_ET=(Q(LPNUT2+3)+MET_VEC(1))**2 +
     1   (Q(LPNUT2+4)+MET_VEC(2))**2
        PNUT2_ET=SQRT(PNUT2_ET)
      ENDIF
C
C
C *** PMUO - find all good muons; exit if there aren't sufficient
C
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.GT.0)
        IF(TOP_LEPTONS_GOOD_MUON(LPMUO)) THEN
          I_MU=I_MU+1
          IF(I_MU.LT.6) LPMUO_VEC(I_MU)=LPMUO
        ENDIF
        LPMUO=LQ(LPMUO)
      ENDDO
      IF(I_MU.LT.NOMUON_MIN) GO TO 999
C
C *** JETS - find all good jets; exit if there aren't sufficient
C *** Calculate HT--sum ET in jets
C
      DO I = 1,10
        JET_PT(I) = 0.
      ENDDO
      LJETS=GZJETS()
      DO WHILE(LJETS.GT.0)
        IF(TOP_LEPTONS_GOOD_JET(LJETS)) THEN
          I_JT=I_JT+1
          IF(I_JT.LT.11)THEN
            LJETS_VEC(I_JT)=LJETS
C
C *** Use jet corrections if requested
C
            IF(CORR_JETS) THEN
              CALL TOP_LEPTONS_CORR_JETPARM(LJETS,JETS_E,JETS_ET,
     1          JETS_EX,JETS_EY,JETS_EZ,JETS_PHI,JETS_ETA,IER)
              IF(IER.GT.0) THEN
                JET_PT(I_JT)=JETS_ET
                JET_ETA(I_JT)=JETS_ETA
              ELSE
                JET_PT(I_JT)=Q(LJETS+6)
                JET_ETA(I_JT)=Q(LJETS+9)
              ENDIF
            ELSE
              JET_PT(I_JT) = Q(LJETS+6)
              JET_ETA(I_JT)=Q(LJETS+9)
            ENDIF
            IF((JET_PT(I_JT).GE.HT_JETPT_MIN).AND.
     1        (ABS(JET_ETA(I_JT)).LE.HT_JETETA_MAX)) THEN
                HT = HT + JET_PT(I_JT)
            ENDIF
          ENDIF
        ELSE
        ENDIF
        LJETS=LQ(LJETS)
      ENDDO
      IF(I_JT.LT.NOJT_MIN) GO TO 999
      IF(I_JT.GT.10) I_JT = 10    !Set max limit on number of jets
      IF(I_JT.GE.2) THEN
C
C *** Calculate shape variables
C
        CALL TOP_LEPTONS_EVENT_SHAPE(I_JT,LJETS_VEC,SPHER,PLAN,
     1                 GSPHER,GAPLAN,GY,EMAXSH,ETMAXSH,EFOURSH,IERR)
        IF(DO_SHAPE_JET_DROP .AND. IERR.NE.0) GOTO 999
C
C *** Sort jets in descending ET...
C ***      CERNLIB routine M101
C
        MODE = 1   !Real sort  
        NWAY = 1   !Descending sort
        NSORT = 0  !Sort first I_JT elements of Jet_PT
        CALL SORTZV(JET_PT,JET_PT_INDEX,I_JT,MODE,NWAY,NSORT)
      ENDIF
C-----------------------------------------------------------------------
C
C **** Apply selection cuts ****
C
C *** Look for correct number of isolated and nonisolated muons
C
      IF (DO_ISOL_CUT) THEN
        CALL TOP_LEPTONS_FIND_ISOLMU(MIN(I_MU,5),LPMUO_VEC,I_MU_ISOL,
     1                               LPMUO_VEC_ISOL,I_JT,ISO_MODE)
        DO I = 1,I_MU_ISOL
          IF(Q(LPMUO_VEC_ISOL(I)+14).GT.MUON1_PTMIN) THEN
            NUM_GOOD_PRI_MUON = NUM_GOOD_PRI_MUON + 1
          ENDIF
        ENDDO
        IF(NUM_GOOD_PRI_MUON.LT.NOMU_ISO_MIN) GO TO 999
        IF(NUM_GOOD_PRI_MUON.GT.NOMU_ISO_MAX) GO TO 999
        CALL TOP_LEPTONS_FIND_ISOLMU(I_MU,LPMUO_VEC,I_MU_NONISOL,
     1                               LPMUO_VEC_NONISOL,I_JT,NONISO_MODE)
        DO I = 1,I_MU_NONISOL
          IF(Q(LPMUO_VEC_NONISOL(I)+14).GT.MUON2_PTMIN) THEN
            NUM_GOOD_SEC_MUON = NUM_GOOD_SEC_MUON + 1
          ENDIF
        ENDDO
        IF(NUM_GOOD_SEC_MUON.LT.NOMU_NONISO_MIN) GO TO 999
        IF(NUM_GOOD_SEC_MUON.GT.NOMU_NONISO_MAX) GO TO 999
      ELSE
        DO I = 1,I_MU
          IF(Q(LPMUO_VEC(I)+14).GT.MUON1_PTMIN) THEN
            NUM_GOOD_PRI_MUON = NUM_GOOD_PRI_MUON + 1
          ENDIF
        ENDDO
        I_MU_ISOL=I_MU
        CALL UCOPY(LPMUO_VEC(1),LPMUO_VEC_ISOL(1),I_MU)
      ENDIF
      IF((NUM_GOOD_PRI_MUON+NUM_GOOD_SEC_MUON).LT.NOMUON_MIN) GO TO 999
      IF((NUM_GOOD_PRI_MUON+NUM_GOOD_SEC_MUON).GT.NOMUON_MAX) GO TO 999
C
C *** Cut on primary muon back-to-back with highest ET jet
C
      IF (I_MU_ISOL.GE.1) THEN
        MU_Phi = Q(LPMUO_VEC_ISOL(1) + 17)
        MU_Neg_Phi = MU_Phi + PI
        IF (MU_Neg_Phi .GT. TWOPI) MU_Neg_Phi = MU_Neg_Phi - TWOPI
        Jet_Phi = Q(LJETS_VEC(JET_PT_INDEX(1)) + 8)
        DPHI = ABS(Jet_Phi - Mu_Neg_Phi)
        IF(DPHI.GT.PI) DPHI=TWOPI-DPHI
        IF (DPHI .LT. B2B_DPHIMIN) GO TO 999
        IF (DPHI .GT. B2B_DPHIMAX) GO TO 999
      ENDIF
C
C *** Jet multiplicity and Pt cuts
C
      DO I = 1,NOJT_MIN
        IF(JET_PT(Jet_PT_Index(I)).LT.JET_PTMIN(I)) GO TO 999
      ENDDO
      IF (NOJT_MAX.LE.9) THEN
        IF(I_JT.GE.(NOJT_MAX+1))THEN
          IF(JET_PT(Jet_PT_Index(NOJT_MAX+1)).GT.PTMAX_NEXTJET)GO TO 999
        ENDIF
      ENDIF
C
C *** HT cut
C
      IF(HT.LT.HT_MIN) GO TO 999
C
C *** Missing ET cut
C
C      IF (LPNUT3.EQ.0) GO TO 999  !Force PNUT to exist
      IF(PNUT3_ET.LT.PNUT3_ETMIN) GO TO 999
      IF(PNUT2_ET.LT.PNUT2_ETMIN) GO TO 999
C
C *** Shape cuts
C
      IF (GAPLAN.LT.APLAN_MIN.AND.APLAN_MIN.GE.0.) GO TO 999
C
C *** Good candidate
C
      IFGOOD=.TRUE.
      GO TO 999
C----------------------------------------------------------------------
  999 RETURN
      END
