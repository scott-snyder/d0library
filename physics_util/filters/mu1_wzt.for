      LOGICAL FUNCTION MU1_WZT
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for the single muon offline
C-                         filter for W,Z and Top
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: MU1_WZT RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  12-Dec-1992   Darien Wood
C-   Updated  July 1993 Cecilia E. Gerber
C-   Updated  10-Jan-1994 C. Gerber, changed for run 1B streaming
C-   Updated   7-MAY-1995   Darien Wood      added HFRACT cut  
C-   Updated  13-MAY-1995   Jamal N. Tarazi  removed bad_event writeout 
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL COSREJ,XOCT,PASSED,FIRST,MU1_WZT_EOJ
      INTEGER IER,IFW4CUT,LPARH,NPMUO,IPMUO,LPMUO,LMUOT
      REAL ETACUT,PTCUT,CF_CAL,EF_CAL,TFLOATCUT
      REAL ETAMU,PTMU,ECAL_1NN,BV_IMPACT,TFLOAT,CIMP_BV
      REAL    MU1_STAT(20),MU1_SUMRY(20)
      REAL HFRACT,HFRACTCUT,EFRACTH1,EFRACTH1CUT
      INTEGER IFW4,IFW2,IQUAD,I
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
C   Get parameters from MU1_WZT.RCP
        CALL INRCP('MU1_WZT_RCP',IER)
        IF(IER.NE.0) THEN
          CALL ERRMSG('MU1_WZT','MU1_WZT_RCP',
     &        'ERROR READING MU1_WZT RCP FILE','W')
          STOP 'MU1_WZT_RCP not found'
        ELSE
          CALL EZPICK('MU1_WZT_RCP')
          CALL EZGET('ETA_MUW', ETACUT, IER)
          CALL EZGET('PT_MUW', PTCUT, IER)
          CALL EZGET('IFW4_MUW', IFW4CUT, IER)
          CALL EZGET('MUCTAG_MUW', COSREJ, IER)
          CALL EZGET('XOCT_MUW', XOCT, IER)
          CALL EZGET('IMP_BV_MUW', CIMP_BV, IER)
          CALL EZGET('CF_CAL_MUW', CF_CAL, IER)
          CALL EZGET('EF_CAL_MUW', EF_CAL, IER)
          CALL EZGET('TFLOAT_MUW', TFLOATCUT, IER)
          CALL EZGET('HFRACT_MUW', HFRACTCUT, IER)
          CALL EZGET('EFRACTH1_MUW', EFRACTH1CUT, IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('MU1_WZT','MU1_WZT_RCP',
     &        'ERROR GETTING MU1_WZT RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MU1_STAT,20)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      PASSED = .FALSE.
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
      IF(NPMUO.GT.0) THEN
        DO IPMUO=1,NPMUO
          MU1_STAT(4) = MU1_STAT(4) + 1.
          LPMUO = GZPMUO(IPMUO)
C protection against certain badly RECOed events 
          IF (IQ(LPMUO+10).EQ.32769) THEN
            MU1_WZT = .FALSE.
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
C do cuts...
            IF(PTMU.LT.PTCUT) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT) GOTO 1000
            IF(IFW4.GT.IFW4CUT) GOTO 1000
            IF (ABS(BV_IMPACT).GT.CIMP_BV) GOTO 1000
            IF (TFLOAT.GT.TFLOATCUT) GOTO 1000
            IF(COSREJ) THEN
              IF(BTEST(IFW2,6)) GOTO 1000
              IF(BTEST(IFW2,7)) GOTO 1000
            ENDIF
            IF(XOCT) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
C CF cuts
            IF (ABS(ETAMU).LE.1) THEN
              IF (ECAL_1NN.LT.CF_CAL) GOTO 1000
            ENDIF
C EF cuts
            IF (ABS(ETAMU).GT.1) THEN
              IF (ECAL_1NN.LT.EF_CAL) GOTO 1000
            ENDIF
C MTC cuts
            IF(HFRACT.LT.HFRACTCUT) GOTO 1000
            IF(EFRACTH1.LE.EFRACTH1CUT) GOTO 1000
C
            PASSED = .TRUE.
C keep statistics for passed tracks: ALL,CF,EFN,EFS (5,6,7,8)
            MU1_STAT(5) = MU1_STAT(5) + 1.
            IQUAD = IQ(LMUOT+3)
            IF(IQUAD.LE.4) THEN
              MU1_STAT(6) = MU1_STAT(6) + 1.
            ELSEIF(IQUAD.GE.9) THEN
              MU1_STAT(7) = MU1_STAT(7) + 1.
            ELSE
              MU1_STAT(8) = MU1_STAT(8) + 1.
            ENDIF
C
 1000       CONTINUE
          ENDIF
        ENDDO
      ENDIF
C
C see if event passed selection
C keep events with at least one good high pt muon
C
      IF(PASSED) MU1_STAT(2) = MU1_STAT(2) + 1.
      MU1_WZT = PASSED
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY MU1_WZT_EOJ(MU1_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Some end of job statistics for W filter
C-
C- MU1_SUMRY  1 = total number of events on which filter was run
C-            2 = total number of candidate events
C-            3 = fraction of events accepted (%)
C-            4 = total number of muons for which filter was run
C-            5 = fraction of mouns accepted (%)
C-            6 = fraction of accepted muons in CF (%)
C-            7 = fraction of accepted muons in EFN (%)
C-            8 = fraction of accepted muons in EFS (%)
C-
C-
C----------------------------------------------------------------------
      MU1_WZT_EOJ=.TRUE.
      CALL UCOPY(MU1_STAT,MU1_SUMRY,20)
      IF(MU1_STAT(1).GT.0.) MU1_SUMRY(3)=MU1_STAT(2)/MU1_STAT(1)*100.
      IF(MU1_STAT(4).GT.0.) MU1_SUMRY(5)=MU1_STAT(5)/MU1_STAT(4)
      IF(MU1_STAT(5).GT.0.) THEN
        DO I=6,8
          MU1_SUMRY(I)=MU1_STAT(I)/MU1_STAT(5)*100.
        ENDDO
      ENDIF

      RETURN
      END
