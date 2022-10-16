      LOGICAL FUNCTION TOP_MUJ
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for Top -> Mu + Jet
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: TOP_MUJ RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  1-Feb-1993   Joey Thompson
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL COSREJ1,XOCT1,PASSED,FIRST,TOP_MUJ_EOJ
      LOGICAL COSREJ2,XOCT2
      INTEGER IER,LPARH,NPMUO,IPMUO,LPMUO,NS,LMUOT
      INTEGER LPNUT,GZPNUT,LJETS,GZJETS
      INTEGER IFW4CUT1,NCD_OR1,IFW4CUT2,NCD_OR2
      REAL ETACUT1,PTCUT1,CIMP_RZ1,CIMP_XY1,CAL_OR1,CISO_OR1
      REAL ETACUT2,PTCUT2
      REAL ETAMU,PTMU,ECAL_2NN,CALISO_2NN,RZ_IMPACT,XY_IMPACT
      REAL COS_TRAN
      INTEGER IFW4,IFW2,NCDMATCH,NGOOD_LOOSE,NGOOD_TIGHT
      REAL    MUJ_STAT(20),MUJ_SUMRY(20)
C
      INTEGER ICHOICE
      INTEGER NJETS_MIN_WITHMUL,NJETS_MIN_WITHMET
      INTEGER NJETS_WITHMUL,NJETS_WITHMET
      REAL    JETS_MINET_WITHMUL,JETS_MINET_WITHMET,TEMPLATE(5,4),ET
      INTEGER PNUT
      REAL    MET_MIN,MISSET
      INTEGER GZPMUO,GZPARH
      LOGICAL EZERR
C
      EXTERNAL GZPMUO,GZPARH,EZERR
C
      DATA FIRST /.TRUE./
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C   Get parameters from TOP_MUJ.RCP
        CALL INRCP('TOP_MUJ_RCP',IER)
        IF (IER.NE.0) THEN 
          CALL ERRMSG('TOP_MUJ','TOP_MUJ_RCP',
     &        'ERROR READING TOP_MUJ RCP FILE','W')
          STOP 'TOP_MUJ_RCP not found'
        ELSE
          CALL EZPICK('TOP_MUJ_RCP')
C parameters for first (tight) muon
          IF (IER.EQ.0) CALL EZGET('ETA_MJMUT', ETACUT1, IER)
          IF (IER.EQ.0) CALL EZGET('PT_MJMUT', PTCUT1, IER)
          IF (IER.EQ.0) CALL EZGET_i('IFW4_MJMUT', IFW4CUT1, IER)
          IF (IER.EQ.0) CALL EZGET_l('MUCTAG_MJMUT', COSREJ1, IER)
          IF (IER.EQ.0) CALL EZGET_l('XOCT_MJMUT', XOCT1, IER)
          IF (IER.EQ.0) CALL EZGET('IMP_RZ_MJMUT', CIMP_RZ1,IER)
          IF (IER.EQ.0) CALL EZGET('IMP_XY_MJMUT', CIMP_XY1, IER)
          IF (IER.EQ.0) CALL EZGET_i('CD_OR_MJMUT', NCD_OR1, IER)
          IF (IER.EQ.0) CALL EZGET('CAL_OR_MJMUT', CAL_OR1, IER)
          IF (IER.EQ.0) CALL EZGET('ISO_OR_MJMUT', CISO_OR1, IER)
C parameters for second (loose) muon
          IF (IER.EQ.0) CALL EZGET('ETA_MJMUL', ETACUT2, IER)
          IF (IER.EQ.0) CALL EZGET('PT_MJMUL', PTCUT2, IER)
          IF (IER.EQ.0) CALL EZGET_l('MUCTAG_MJMUL', COSREJ2, IER)
          IF (IER.EQ.0) CALL EZGET_i('IFW4_MJMUL', IFW4CUT2, IER)
          IF (IER.EQ.0) CALL EZGET_l('XOCT_MJMUL', XOCT2, IER)
C parameters for jets
          IF (IER.EQ.0) CALL EZGET_i('JETS_ALGORITHM',ICHOICE,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_MINET_WITHMUL',
     &      JETS_MINET_WITHMUL,IER)
          IF (IER.EQ.0) CALL EZGET('JETS_MINET_WITHMET',
     &      JETS_MINET_WITHMET,IER)
          IF (IER.EQ.0) CALL EZGET_i('NJETS_MIN_WITHMUL',
     &      NJETS_MIN_WITHMUL,IER)
          IF (IER.EQ.0) CALL EZGET_i('NJETS_MIN_WITHMET',
     &      NJETS_MIN_WITHMET,IER)
C parameters for Missing ET
          IF (IER.EQ.0) CALL EZGET_i('PNUT',PNUT,IER)
          IF (IER.EQ.0) CALL EZGET('MET_MIN',MET_MIN,IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_MUJ','TOP_MUJ_RCP',
     &        'ERROR GETTING TOP_MUJ RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MUJ_STAT,20)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      PASSED = .FALSE.
      MUJ_STAT(1) = MUJ_STAT(1) + 1.
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
      IF(NPMUO.GE.1) THEN
        DO IPMUO=1,NPMUO
          LPMUO = GZPMUO(IPMUO)
          IF(LPMUO.GT.0) THEN
C get muon information from PMUO bank
            ETAMU = Q(LPMUO+16)
            PTMU = Q(LPMUO+14)
            IFW4 = IQ(LPMUO+9)
            IFW2 = IQ(LPMUO+44)
            NCDMATCH = IQ(LPMUO+6)
            ECAL_2NN = Q(LPMUO+34)
            CALISO_2NN = Q(LPMUO+30)
            RZ_IMPACT = Q(LPMUO+41)
C impact param, non-bend view
            XY_IMPACT = 0.
            NS = IQ(LPMUO-2)
            LMUOT = LQ(LPMUO-NS-1)
            IF(LMUOT.GT.0) THEN
              COS_TRAN = SQRT(Q(LMUOT+17)**2+Q(LMUOT+18)**2)
              IF(COS_TRAN.GT.0.) THEN
                XY_IMPACT = (Q(LMUOT+11)*Q(LMUOT+18) - 
     &            Q(LMUOT+12)*Q(LMUOT+17))/COS_TRAN
              ENDIF
            ENDIF  
C do loose cuts...
            IF(PTMU.LT.PTCUT2) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT2) GOTO 1000
            IF(IFW4.GT.IFW4CUT2) GOTO 1000
            IF(COSREJ2) THEN
              IF(BTEST(IFW2,6)) GOTO 1000
              IF(BTEST(IFW2,7)) GOTO 1000
            ENDIF
            IF(XOCT2) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
C loose muon found
            NGOOD_LOOSE = NGOOD_LOOSE+1
c do tight cuts
            IF(PTMU.LT.PTCUT1) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT1) GOTO 1000
            IF(IFW4.GT.IFW4CUT1) GOTO 1000
            IF(COSREJ1) THEN
              IF(BTEST(IFW2,6)) GOTO 1000
              IF(BTEST(IFW2,7)) GOTO 1000
            ENDIF
            IF(XOCT1) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
            IF(RZ_IMPACT.GT.CIMP_RZ1) GOTO 1000
            IF(ABS(XY_IMPACT).GT.CIMP_XY1) GOTO 1000
C or of three conditions
            IF(NCDMATCH.LT.NCD_OR1 .AND. ECAL_2NN.LT.CAL_OR1 .AND.
     &        CALISO_2NN.LT.CISO_OR1) GOTO 1000
C
C tight muon found
            NGOOD_TIGHT = NGOOD_TIGHT + 1
C
 1000       CONTINUE    
          ENDIF
        ENDDO
      ENDIF
C set jet algorithm
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C count jets
      NJETS_WITHMUL = 0
      NJETS_WITHMET = 0
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0)
          ET =SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
          IF (ET.GE.JETS_MINET_WITHMUL) NJETS_WITHMUL=NJETS_WITHMUL+1
          IF (ET.GE.JETS_MINET_WITHMET) NJETS_WITHMET=NJETS_WITHMET+1
          LJETS=LQ(LJETS)          ! pointer to next jet
        ENDDO
      ENDIF
      CALL RESET_CAPH
C Find missing ET
      MISSET = 0.
      IF (PNUT.EQ.1) THEN
        LPNUT=GZPNUT(1)
      ELSE IF (PNUT.EQ.2) THEN
        LPNUT=GZPNUT(2)
        IF(LPNUT.EQ.0) LPNUT=GZPNUT(1)
      ELSE IF (PNUT.EQ.3) THEN
        LPNUT=GZPNUT(3)
        IF(LPNUT.EQ.0) LPNUT=GZPNUT(2)
        IF(LPNUT.EQ.0) LPNUT=GZPNUT(1)
      ELSE IF (PNUT.EQ.4) THEN
        LPNUT=GZPNUT(4)
        IF(LPNUT.EQ.0) LPNUT=GZPNUT(2)
        IF(LPNUT.EQ.0) LPNUT=GZPNUT(1)
      ENDIF
      IF (LPNUT.NE.0) MISSET = Q(LPNUT+7)
C Require at least one tight
C     OR:
C         One loose plus jets (tight is a subset of loose)
C     OR:
C         Missing ET plus jets
C 
      IF(NGOOD_TIGHT.GE.1) THEN
        PASSED = .TRUE.
        MUJ_STAT(4) = MUJ_STAT(4)+1
      ENDIF
      IF(NGOOD_LOOSE.GE.1 .AND. NJETS_WITHMUL.GE.NJETS_MIN_WITHMUL) THEN
        PASSED = .TRUE.
        MUJ_STAT(5) = MUJ_STAT(5)+1
      ENDIF
      IF(MISSET.GE.MET_MIN .AND. NJETS_WITHMET.GE.NJETS_MIN_WITHMET)THEN
        PASSED = .TRUE.
        MUJ_STAT(6) = MUJ_STAT(6)+1
      ENDIF
C
C see if event passed selection
C
      IF(PASSED) MUJ_STAT(2) = MUJ_STAT(2) + 1.
      TOP_MUJ = PASSED
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY TOP_MUJ_EOJ(MUJ_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job statistics for TOP->MUJET filter
C-
C- MUJ_SUMRY  1 = total number of events on which filter was run 
C-            2 = total number of candidate events
C-            3 = fraction of events accepted (%)
C-            4=  fraction of accepted events with 1 tight muon  
C-            5=  fraction of accepted events with 1 loose muon + jets   
C-            6=  fraction of accepted events with 1 MET + jets  
C-      
C-
C----------------------------------------------------------------------
      TOP_MUJ_EOJ=.TRUE.
      CALL UCOPY(MUJ_STAT,MUJ_SUMRY,20)
      IF(MUJ_STAT(1).GT.0.) MUJ_SUMRY(3)=MUJ_STAT(2)/MUJ_STAT(1)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(4)=MUJ_STAT(4)/MUJ_STAT(2)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(5)=MUJ_STAT(5)/MUJ_STAT(2)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(6)=MUJ_STAT(6)/MUJ_STAT(2)*100.
C      
      RETURN
      END
