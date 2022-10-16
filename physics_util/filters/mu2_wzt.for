      LOGICAL FUNCTION MU2_WZT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for the dimuon offline
C-                         filter for W,Z and Top
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: MU2_WZT RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  12-Dec-1992   Darien Wood
C-   Updated July 1993  Cecilia E. Gerber
C-   Updated  10-Jan-1994 C. Gerber, changed for run 1B streaming
C-   Updated  13-MAY-1995   Darien Wood       added HFRACT cut
C-   Updated  13-MAY-1995   Jamal N. Tarazi   removed bad_event writeout
C----------------------------------------------------------------------
 
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL COSREJ1,XOCT1,PASSED,FIRST,MU2_WZT_EOJ
      LOGICAL XOCT2
      INTEGER IER,LPARH,NPMUO,IPMUO,LPMUO,LMUOT
      INTEGER IFW4CUT1,IFW4CUT2
      REAL ETACUT1,PTCUT1,CIMP_BV1,CF_CAL1,EF_CAL1
      REAL ETACUT2,PTCUT2,TFLOAT1,TFLOAT
      REAL ETAMU,PTMU,ECAL_1NN,BV_IMPACT
      REAL HFRACT,HFRACTCUT,EFRACTH1,EFRACTH1CUT
      INTEGER IFW4,IFW2,NGOOD_LOOSE,NGOOD_TIGHT
      INTEGER NTIGHT_HIGHPT, NTIGHT_LOWPT
      INTEGER NLOOSE_HIGHPT, NLOOSE_LOWPT
      REAL    MU2_STAT(20),MU2_SUMRY(20)
C
      INTEGER GZPMUO,GZPARH
      LOGICAL EZERR
C
      EXTERNAL GZPMUO,GZPARH,EZERR
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C   Get parameters from MU2_WZT.RCP
        CALL INRCP('MU2_WZT_RCP',IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('MU2_WZT','MU2_WZT_RCP',
     &        'ERROR READING MU2_WZT RCP FILE','W')
          STOP 'MU2_WZT_RCP not found'
        ELSE
          CALL EZPICK('MU2_WZT_RCP')
C parameters for first (tight) muon
          CALL EZGET('ETA_MUZ1', ETACUT1, IER)
          CALL EZGET('PT_MUZ1', PTCUT1, IER)
          CALL EZGET_i('IFW4_MUZ1', IFW4CUT1, IER)
          CALL EZGET_l('MUCTAG_MUZ1', COSREJ1, IER)
          CALL EZGET_l('XOCT_MUZ1', XOCT1, IER)
          CALL EZGET('IMP_BV_MUZ1', CIMP_BV1,IER)
          CALL EZGET('CF_CAL_MUZ1', CF_CAL1, IER)
          CALL EZGET('EF_CAL_MUZ1', EF_CAL1, IER)
          CALL EZGET('TFLOAT_MUZ1', TFLOAT1, IER)
          CALL EZGET('HFRACT_MUZ1', HFRACTCUT, IER)
          CALL EZGET('EFRACTHG1_MUZ1', EFRACTH1CUT, IER)
C parameters for second (loose) muon
          CALL EZGET('ETA_MUZ2', ETACUT2, IER)
          CALL EZGET('PT_MUZ2', PTCUT2, IER)
          CALL EZGET_i('IFW4_MUZ2', IFW4CUT2, IER)
          CALL EZGET_l('XOCT_MUZ2', XOCT2, IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('MU2_WZT','MU2_WZT_RCP',
     &        'ERROR GETTING MU2_WZT RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MU2_STAT,20)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      PASSED = .FALSE.
      MU2_STAT(1) = MU2_STAT(1) + 1.
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
      IF(NPMUO.GE.2) THEN
        DO IPMUO=1,NPMUO
          LPMUO = GZPMUO(IPMUO)
C protection against badly RECOed events
          IF (IQ(LPMUO+10).EQ.32769) THEN
            MU2_WZT = .FALSE.
            GOTO 999
          ENDIF
          IF(LPMUO.GT.0) THEN
C get muon information from PMUO bank
            ETAMU = Q(LPMUO+16)
            PTMU = Q(LPMUO+14)
            IFW4 = IQ(LPMUO+9)
            IFW2 = IQ(LPMUO+44)
            ECAL_1NN = Q(LPMUO+84)
            BV_IMPACT = Q(LPMUO+56)
            TFLOAT = Q(LPMUO+24)
            HFRACT = Q(LPMUO+94)
            EFRACTH1 = Q(LPMUO+98)
            LMUOT = LQ(LPMUO-IQ(LPMUO-2)-1)
C do loose cuts...
            IF(PTMU.LT.PTCUT2) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT2) GOTO 1000
            IF(IFW4.GT.IFW4CUT2) GOTO 1000
            IF(XOCT2) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
C loose muon found
            NGOOD_LOOSE = NGOOD_LOOSE+1
            IF(PTMU.GT.PTCUT1) THEN
              NLOOSE_HIGHPT = NLOOSE_HIGHPT + 1
            ELSE
              NLOOSE_LOWPT = NLOOSE_LOWPT + 1
            ENDIF
c do tight cuts
            IF(PTMU.LT.PTCUT2) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT1) GOTO 1000
            IF(IFW4.GT.IFW4CUT1) GOTO 1000
            IF (TFLOAT.GT.TFLOAT1) GOTO 1000
            IF(COSREJ1) THEN
              IF(BTEST(IFW2,6)) GOTO 1000
              IF(BTEST(IFW2,7)) GOTO 1000
            ENDIF
            IF(XOCT1) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
            IF(ABS(BV_IMPACT).GT.CIMP_BV1) GOTO 1000
C CF cuts
            IF (ABS(ETAMU).LE.1) THEN
              IF (ECAL_1NN.LT.CF_CAL1) GOTO 1000
            ENDIF
C EF cuts
            IF (ABS(ETAMU).GT.1) THEN
              IF (ECAL_1NN.LT.EF_CAL1) GOTO 1000
            ENDIF
C MTC cuts
            IF(HFRACT.LT.HFRACTCUT) GOTO 1000
            IF(EFRACTH1.LE.EFRACTH1CUT) GOTO 1000
C
C tight muon found
            NGOOD_TIGHT = NGOOD_TIGHT + 1
            IF(PTMU.GT.PTCUT1) THEN
              NTIGHT_HIGHPT = NTIGHT_HIGHPT + 1
            ELSE
              NTIGHT_LOWPT = NTIGHT_LOWPT + 1
            ENDIF
C
 1000       CONTINUE
          ENDIF
        ENDDO
C require at least one tight and one other loose
C (tight is a subset of loose)
        IF(NGOOD_LOOSE.GE.2 .AND. NGOOD_TIGHT.GE.1) THEN
          IF ((NTIGHT_HIGHPT.GE.1.AND.NLOOSE_LOWPT.GE.1).OR.
     &      (NTIGHT_LOWPT.GE.1.AND.NLOOSE_HIGHPT.GE.1).OR.
     &      (NLOOSE_HIGHPT.GE.2.AND.NTIGHT_HIGHPT.GE.1))
     &      PASSED = .TRUE.
        ENDIF
        IF(NTIGHT_HIGHPT.GE.2) MU2_STAT(4) = MU2_STAT(4) + 1.
      ENDIF
C
C see if event passed selection
C
      IF(PASSED) MU2_STAT(2) = MU2_STAT(2) + 1.
      MU2_WZT = PASSED
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY MU2_WZT_EOJ(MU2_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for W filter
C-
C- MU2_SUMRY  1 = total number of events on which filter was run
C-            2 = total number of candidate events
C-            3 = fraction of events accepted (%)
C-            4=  fraction of accepted events with 2 tight muons
C-
C-
C----------------------------------------------------------------------
      MU2_WZT_EOJ=.TRUE.
      CALL UCOPY(MU2_STAT,MU2_SUMRY,20)
      IF(MU2_STAT(1).GT.0.) MU2_SUMRY(3)=MU2_STAT(2)/MU2_STAT(1)*100.
      IF(MU2_STAT(2).GT.0.) MU2_SUMRY(4)=MU2_STAT(4)/MU2_STAT(2)*100.
C
      RETURN
      END
