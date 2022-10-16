      LOGICAL FUNCTION TOP_TOP_BBBAR
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for the dimuon offline
C-                         filter for Top
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: TOP_TOP_BBBAR RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  23-OCT-1995   Jeffrey Bantly   variation of MU1_WZT.FOR
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:PI.DEF'
C
      LOGICAL COSREJ1,XOCT1,PASSED,FIRST,TOP_TOP_BBBAR_EOJ
      LOGICAL XOCT2,PASSED1,PASSED2,PASSED3
      INTEGER IER,LPARH,NPMUO,IPMUO,LPMUO,LMUOT
      INTEGER IFW4CUT1,IFW4CUT2,QUADCUT1,QUADCUT2
      REAL ETACUT1,PTCUT1,CIMP_BV1,CF_CAL1,EF_CAL1
      REAL ETACUT2,PTCUT2,TFLOAT1,TFLOAT
      REAL ETAMU,PTMU,ECAL_1NN,BV_IMPACT,MUCTAG,PHIMU
      REAL HFRACT,HFRACTCUT1,HFRACTCUT2,EFRACTH1,EFRACTH1CUT
      REAL ETAMU_SAVE(10),PHIMU_SAVE(10),MUCTAG_SAVE(10)
      INTEGER IFW4,IFW2,NGOOD_LOOSE,NGOOD_TIGHT
      INTEGER NTIGHT_HIGHPT, NTIGHT_LOWPT
      INTEGER NLOOSE_HIGHPT, NLOOSE_LOWPT
      REAL    MU1_STAT(20),MU1_SUMRY(20)
      LOGICAL CDMTCH_EF
      REAL DPHI_EF_CUT,DTHETA_EF_CUT,CHISQ_CF_CUT
      REAL DPHI,DTHETA,CHISQ,BDL_CUT
      INTEGER IQUAD,N_CD_MATCH,JBIT
      REAL    PTV,PTFIX
      EXTERNAL PTFIX,JBIT
      INTEGER LVERH,LVERT,NPVERT,GZVERH
      EXTERNAL GZVERH
      LOGICAL HIGH_FIX,LOW_FIX
      LOGICAL FILTER_MUJET
      CHARACTER*32 FILT_NAME
      LOGICAL L2NAME_PASSED
      EXTERNAL L2NAME_PASSED
      INTEGER NJETS, NJETS2, LJETS, IERR
      REAL CONE_PARAMS(3),PT
C
      INTEGER GZPMUO,GZPARH,GZJETS
      LOGICAL EZERR
C
      EXTERNAL GZPMUO,GZPARH,GZJETS,EZERR
C
      DATA FIRST /.TRUE./
      DATA CONE_PARAMS /1.0, 6.0, 0.5/
C----------------------------------------------------------------------
      IF (FIRST) THEN
        OPEN(UNIT=12, FILE='MU1_BAD_EVENTS.OUT',STATUS='NEW')
C
C   Get parameters from TOP_TOP_BBBAR.RCP
        CALL INRCP('TOP_TOP_BBBAR_RCP',IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('TOP_TOP_BBBAR','TOP_TOP_BBBAR_RCP',
     &        'ERROR READING TOP_TOP_BBBAR RCP FILE','W')
          STOP 'TOP_TOP_BBBAR_RCP not found'
        ELSE
          CALL EZPICK('TOP_TOP_BBBAR_RCP')
C parameters for first (tight) muon
          CALL EZGET('ETA_MUZ1', ETACUT1, IER)
          CALL EZGET_i('QUAD_MUZ1', QUADCUT1, IER)
          CALL EZGET('PT_MUZ1', PTCUT1, IER)
          CALL EZGET_i('IFW4_MUZ1', IFW4CUT1, IER)
          CALL EZGET_l('MUCTAG_MUZ1', COSREJ1, IER)
          CALL EZGET_l('XOCT_MUZ1', XOCT1, IER)
          CALL EZGET('IMP_BV_MUZ1', CIMP_BV1,IER)
          CALL EZGET('CF_CAL_MUZ1', CF_CAL1, IER)
          CALL EZGET('EF_CAL_MUZ1', EF_CAL1, IER)
          CALL EZGET('TFLOAT_MUZ1', TFLOAT1, IER)
          CALL EZGET('HFRACT_MUZ1', HFRACTCUT1, IER)
          CALL EZGET('EFRACTHG1_MUZ1', EFRACTH1CUT, IER)
          CALL EZGET_l('CDMTCH_EF_MUZ1', CDMTCH_EF, IER)
          CALL EZGET('DPHI_EF_MUZ1', DPHI_EF_CUT, IER)
          CALL EZGET('DTHETA_EF_MUZ1', DTHETA_EF_CUT, IER)
          CALL EZGET('CHISQ_CF_MUZ1', CHISQ_CF_CUT, IER)
          CALL EZGET('BDL_MUZ1', BDL_CUT, IER)
C parameters for second (loose) muon
          CALL EZGET('ETA_MUZ2', ETACUT2, IER)
          CALL EZGET_i('QUAD_MUZ2', QUADCUT2, IER)
          CALL EZGET('PT_MUZ2', PTCUT2, IER)
          CALL EZGET_i('IFW4_MUZ2', IFW4CUT2, IER)
          CALL EZGET_l('XOCT_MUZ2', XOCT2, IER)
          CALL EZGET('HFRACT_MUZ2', HFRACTCUT2, IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_TOP_BBBAR','TOP_TOP_BBBAR_RCP',
     &        'ERROR GETTING TOP_TOP_BBBAR RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MU1_STAT,20)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      PASSED = .FALSE.
      PASSED1 = .FALSE.
      PASSED2 = .FALSE.
      PASSED3 = .FALSE.
      MU1_STAT(1) = MU1_STAT(1) + 1.
C
C check pmuo bank
C
      LPARH=GZPARH()
      NPMUO = 0
      IF (LPARH.GT.0) THEN
        NPMUO=IQ(LPARH+2)
      ENDIF
C
C find number of good muons in the event
C
      NGOOD_LOOSE = 0
      NGOOD_TIGHT = 0
      NTIGHT_HIGHPT = 0
      NTIGHT_LOWPT = 0
      NLOOSE_HIGHPT = 0
      NLOOSE_LOWPT = 0
      HIGH_FIX=.FALSE.
      LOW_FIX=.FALSE.
      IF(NPMUO.GE.2) THEN
        DO IPMUO=1,NPMUO
          LPMUO = GZPMUO(IPMUO)
ccccc ---- begin of bad events treatment
ccccc 1) help bad events pass, 2) keep the run, event number of bad events in
ccccc    WZ_3$HROOT:[WZM.LOG]wz2_bad_events.lis, 3) bad events doesn't count in
ccccc    statistics below(they don't count in mu1_stat(*))
          if (iq(lpmuo+10).eq.32769) then
            WRITE(12,*) IQ(LHEAD+6), IQ(LHEAD+9)
            TOP_TOP_BBBAR = .TRUE.
            goto 999
          endif
ccccc ---- end of bad events treatment
          IF(LPMUO.GT.0) THEN
C get muon information from PMUO bank
            N_CD_MATCH = IQ(LPMUO+6)
            IQUAD = IQ(LPMUO+7)
            ETAMU = Q(LPMUO+16)
            PHIMU = Q(LPMUO+17)
            PTMU = Q(LPMUO+14)
            IFW4 = IQ(LPMUO+9)
            IFW2 = IQ(LPMUO+44)
            ECAL_1NN = Q(LPMUO+84)
            BV_IMPACT = Q(LPMUO+56)
            TFLOAT = Q(LPMUO+24)
            HFRACT = Q(LPMUO+94)
            EFRACTH1 = Q(LPMUO+98)
            DPHI = Q(LPMUO+38)/57.29578
            DTHETA = Q(LPMUO+39)/57.29578
            CHISQ = Q(LPMUO+23)
            IF ((JBIT(IQ(LPMUO+44),7).GT.0).OR.
     &              (JBIT(IQ(LPMUO+44),8).GT.0)) THEN
              MUCTAG = 1.
            ELSE
              MUCTAG = 0.
            ENDIF
            LMUOT = LQ(LPMUO-IQ(LPMUO-2)-1)
C check multiple vertices...
            NPVERT=1
            PTV=0
            LVERH=GZVERH()
            IF( LVERH.GT.0 ) NPVERT=IQ(LVERH+2)
            IF( NPVERT.GT.1) THEN  ! Must have mult. verts.
              LVERT=LQ(LVERH-1)
              IF( LVERT.GT.0) PTV=PTFIX(LPMUO,LVERT)
              IF(PTV.GT.PTMU) THEN
                IF(PTMU.LT.PTCUT1.AND.PTV.GE.PTCUT1) HIGH_FIX=.TRUE.
                IF(PTMU.LT.PTCUT2.AND.PTV.GE.PTCUT2) LOW_FIX=.TRUE.
              ENDIF
            ENDIF
C do loose cuts...
            IF(PTMU.LT.PTCUT2.and.PTV.LT.PTCUT2) GOTO 1000
            IF(IQUAD.GT.QUADCUT2) GOTO 1000
            IF(IFW4.GT.IFW4CUT2) GOTO 1000
            IF(XOCT2) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
            IF(HFRACT.LT.HFRACTCUT2) GOTO 1000
C loose muon found
            NGOOD_LOOSE = NGOOD_LOOSE+1
            IF(PTMU.GT.PTCUT1) THEN
              NLOOSE_HIGHPT = NLOOSE_HIGHPT + 1
            ELSE
              NLOOSE_LOWPT = NLOOSE_LOWPT + 1
            ENDIF
c do tight cuts
            IF(PTMU.LT.PTCUT1) GOTO 1000
            IF(IQUAD.GT.QUADCUT1) GOTO 1000
            IF(IFW4.GT.IFW4CUT1) GOTO 1000
            IF(XOCT1) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
C EF cuts
            IF(IQUAD.GT.4) THEN
              IF(IFW4.GT.0) GOTO 1000
            ENDIF
C MTC cuts
            IF(HFRACT.LT.HFRACTCUT1) GOTO 1000
C
C tight muon found
            NGOOD_TIGHT = NGOOD_TIGHT + 1
            IF(NGOOD_TIGHT.GT.10) NGOOD_TIGHT=10
            ETAMU_SAVE(NGOOD_TIGHT)=ETAMU
            PHIMU_SAVE(NGOOD_TIGHT)=PHIMU
            MUCTAG_SAVE(NGOOD_TIGHT)=MUCTAG
            IF(PTMU.GT.PTCUT1) THEN
              NTIGHT_HIGHPT = NTIGHT_HIGHPT + 1
            ELSE
              NTIGHT_LOWPT = NTIGHT_LOWPT + 1
            ENDIF
C
 1000       CONTINUE
          ENDIF
        ENDDO
C
C  Jet checks.  The safest way to insure that all possible signal events
C    are flagged is to:
C       (a) count jets associated with PELC/PPHO's when applying multi. cuts.
C       (b) exclude jets associated w/PELC/PPHO when computing isolation.
C    This is a small (0.3%) effect...  When determining bkg, both signal
C    and bkg streams must thus be used.
C
        CALL SET_CAPH('CONE_JET',CONE_PARAMS,IERR)
        LJETS=GZJETS()
        CALL RESET_CAPH
        NJETS=0
        NJETS2=0
        DO WHILE( LJETS.GT.0 )
          PT=SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          IF( ABS(Q(LJETS+9)).GT.2.5  ) GOTO 20    ! ETA cut
          IF( PT.LT.10 ) GOTO 20                   ! PT1 cut
          NJETS=NJETS+1
          IF( PT.LT.15 ) GOTO 20                   ! PT2 cut
          NJETS2=NJETS2+1
   20     LJETS=LQ(LJETS)
        ENDDO
C
C determine if event passes the mu_jet_* filters
        FILT_NAME = 'MU_JET'
        FILTER_MUJET = L2NAME_PASSED(FILT_NAME)
C
C require at least one tight and one other loose
C (tight is a subset of loose)
        IF(NGOOD_LOOSE.GE.2) THEN
          IF(NJETS.GT.1) PASSED1 = .TRUE.
        ENDIF
        IF(NGOOD_LOOSE.GE.2 .AND. NGOOD_TIGHT.GE.1) THEN
          IF(NJETS.GT.1) PASSED2 = .TRUE.
        ENDIF
        IF(NGOOD_LOOSE.GE.2 .AND. NGOOD_TIGHT.GE.2) THEN
          IF(NJETS.GT.1) PASSED3 = .TRUE.
        ENDIF
        IF(PASSED3) PASSED = .TRUE.
        IF(NTIGHT_HIGHPT.GE.2) MU1_STAT(4) = MU1_STAT(4) + 1.
      ENDIF
C
C see if event passed selection
C
      IF(PASSED)   MU1_STAT(2) = MU1_STAT(2) + 1.
      IF(PASSED .AND. HIGH_FIX) MU1_STAT(5) = MU1_STAT(5) + 1.
      IF(PASSED .AND. LOW_FIX)  MU1_STAT(6) = MU1_STAT(6) + 1.
      IF(PASSED) THEN
        IF(FILTER_MUJET) MU1_STAT(7) = MU1_STAT(7) + 1.
        IF(NJETS.GT.0) MU1_STAT(8)   = MU1_STAT(8) + 1.
        IF(NJETS.GT.1) MU1_STAT(9)   = MU1_STAT(9) + 1.
        IF(NJETS2.GT.0) MU1_STAT(10) = MU1_STAT(10) + 1.
        IF(NJETS2.GT.1) MU1_STAT(11) = MU1_STAT(11) + 1.
      ELSEIF(NPMUO.GE.2) THEN
        IF(NJETS.GT.0) MU1_STAT(12)   = MU1_STAT(12) + 1.
        IF(NJETS2.GT.0) MU1_STAT(13) = MU1_STAT(13) + 1.
        IF(NGOOD_LOOSE.GE.2) THEN
          IF(NJETS.GT.0) MU1_STAT(14)   = MU1_STAT(14) + 1.
          IF(NJETS2.GT.0) MU1_STAT(15) = MU1_STAT(15) + 1.
        ENDIF
      ENDIF
      IF(PASSED1) MU1_STAT(16) = MU1_STAT(16) + 1.
      IF(PASSED2) MU1_STAT(17) = MU1_STAT(17) + 1.
      IF(PASSED3) MU1_STAT(18) = MU1_STAT(18) + 1.
      IF(PASSED1.OR.PASSED3) MU1_STAT(19) = MU1_STAT(19) + 1.
      IF(PASSED2.OR.PASSED3) MU1_STAT(20) = MU1_STAT(20) + 1.
C
      TOP_TOP_BBBAR = PASSED
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY TOP_TOP_BBBAR_EOJ(MU1_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for W filter
C-
C- MU1_SUMRY  1 = total number of events on which filter was run
C-            2 = total number of candidate events
C-            3 = fraction of events accepted (%)
C-            4=  fraction of accepted events with 2 tight muons
C-            5=  total number of tight muons recovered with PTFIX
C-            6=  total number of loose muons recovered with PTFIX
C-            7=  total number of events accepted passing MU_JET filters
C-
C----------------------------------------------------------------------
      TOP_TOP_BBBAR_EOJ=.TRUE.
      CALL UCOPY(MU1_STAT,MU1_SUMRY,20)
      IF(MU1_STAT(1).GT.0.) MU1_SUMRY(3)=MU1_STAT(2)/MU1_STAT(1)*100.
      IF(MU1_STAT(2).GT.0.) MU1_SUMRY(4)=MU1_STAT(4)/MU1_STAT(2)*100.
C      IF(MU1_STAT(5).GT.0.) MU1_SUMRY(5)=MU1_STAT(5)/MU1_STAT(2)*100.
C      IF(MU1_STAT(6).GT.0.) MU1_SUMRY(6)=MU1_STAT(6)/MU1_STAT(2)*100.
C
      RETURN
      END
