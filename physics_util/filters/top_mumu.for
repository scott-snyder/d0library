      LOGICAL FUNCTION TOP_MUMU
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for the dimuon offline
C-                         filter for W,Z and Top
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: TOP_MUMU RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  12-Dec-1992   Darien Wood
C-   Modified 17-JUL-1993   Pushpa Bhat Modified from MU2_WZT for RGE stream
C-                          TOP mumu filter
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL COSREJ1,XOCT1,PASSED,FIRST,TOP_MUMU_EOJ
      LOGICAL COSREJ2,XOCT2
      INTEGER IER,LPARH,NPMUO,IPMUO,LPMUO,NS,LMUOT
      INTEGER IFW4CUT1,NCD_OR1,IFW4CUT2,NCD_OR2
      REAL ETACUT1,PTCUT1,CIMP_RZ1,CAL_OR1,CISO_OR1
      REAL ETACUT2,PTCUT2
      REAL ETAMU,PTMU,ECAL_2NN,CALISO_2NN,RZ_IMPACT
      REAL COS_TRAN
      INTEGER IFW4,IFW2,NCDMATCH,NGOOD_LOOSE,NGOOD_TIGHT
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
C   Get parameters from TOP_MUMU.RCP
        CALL INRCP('TOP_MUMU_RCP',IER)
        IF (IER.NE.0) THEN 
          CALL ERRMSG('TOP_MUMU','TOP_MUMU_RCP',
     &        'ERROR READING TOP_MUMU RCP FILE','W')
          STOP 'TOP_MUMU_RCP not found'
        ELSE
          CALL EZPICK('TOP_MUMU_RCP')
C parameters for first (tight) muon
          CALL EZGET('ETA_MUZ1', ETACUT1, IER)
          CALL EZGET('PT_MUZ1', PTCUT1, IER)
          CALL EZGET('IFW4_MUZ1', IFW4CUT1, IER)
          CALL EZGET('MUCTAG_MUZ1', COSREJ1, IER)
          CALL EZGET('XOCT_MUZ1', XOCT1, IER)
          CALL EZGET('IMP_RZ_MUZ1', CIMP_RZ1,IER)
          CALL EZGET('CD_OR_MUZ1', NCD_OR1, IER)
          CALL EZGET('CAL_OR_MUZ1', CAL_OR1, IER)
          CALL EZGET('ISO_OR_MUZ1', CISO_OR1, IER)
C parameters for second (loose) muon
          CALL EZGET('ETA_MUZ2', ETACUT2, IER)
          CALL EZGET('PT_MUZ2', PTCUT2, IER)
          CALL EZGET('IFW4_MUZ2', IFW4CUT2, IER)
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_MUMU','TOP_MUMU_RCP',
     &        'ERROR GETTING TOP_MUMU RCP VALUES','W')
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
      IF(NPMUO.GE.2) THEN
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
C do loose cuts...
            IF(PTMU.LT.PTCUT2) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT2) GOTO 1000
            IF(IFW4.GT.IFW4CUT2) GOTO 1000
C loose muon found
            NGOOD_LOOSE = NGOOD_LOOSE+1
c do tight cuts
            IF(PTMU.LT.PTCUT1) GOTO 1000
            IF(ABS(ETAMU).GT.ETACUT1) GOTO 1000
            IF(IFW4.GT.IFW4CUT1) GOTO 1000
            IF(XOCT1) THEN
              IF(BTEST(IFW2,8)) GOTO 1000
            ENDIF
            IF(RZ_IMPACT.GT.CIMP_RZ1) GOTO 1000
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
C require at least one tight and one other loose
C (tight is a subset of loose)
        IF(NGOOD_LOOSE.GE.2 .AND. NGOOD_TIGHT.GE.1) PASSED = .TRUE.
        IF(NGOOD_TIGHT.GE.2) MU2_STAT(4) = MU2_STAT(4) + 1.
      ENDIF
C
C see if event passed selection
C
      IF(PASSED) MU2_STAT(2) = MU2_STAT(2) + 1.
      TOP_MUMU = PASSED
C
  999 RETURN
C
C----------------------------------------------------------------------
      ENTRY TOP_MUMU_EOJ(MU2_SUMRY)
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
      TOP_MUMU_EOJ=.TRUE.
      CALL UCOPY(MU2_STAT,MU2_SUMRY,20)
      IF(MU2_STAT(1).GT.0.) MU2_SUMRY(3)=MU2_STAT(2)/MU2_STAT(1)*100.
      IF(MU2_STAT(2).GT.0.) MU2_SUMRY(4)=MU2_STAT(4)/MU2_STAT(2)*100.
C      
      RETURN
      END
