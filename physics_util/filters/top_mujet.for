      LOGICAL FUNCTION TOP_MUJET
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Flags events for Top -> Mu + Jet
C-
C-   Inputs  : none
C-   Outputs :
C-   Controls: TOP_MUJET RCP file
C    Retrun Value: .TRUE. to keep event for filter
C-
C-   Created  1-Feb-1993   Joey Thompson
C-   Modified 27-OCT-1993  Cary Yoshikawa - Tightened signal and added
C-                         cosmic and QCD backgrounds.
C-   Modified 04-OCT-1995  Hui Zhu - add MTC and separate CF/EF code.
C-   Modified 30-OCT-1995  John Hobbs - do not count jets w/photons/pelc
C-    above threshold when forming isolation variable.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL    PI,TWOPI,HALFPI,RAD
      INCLUDE 'D0$INC:PI.INC'
      LOGICAL PASSED_ALL
      LOGICAL PASSED_SIGNAL,PASSED_COSMIC,PASSED_QCD
      LOGICAL PASSED_MU,PASSED_FAKE
      LOGICAL TOP_MUJET_EOJ
      INTEGER LPARH,NPMUO,LPMUO,NS,LMUOT
      INTEGER LJETS,GZJETS
      INTEGER IER
C
      INTEGER MU_QUAD,IFW4
      REAL    MU_P,MU_ETA,MU_PHI,MU_PT
      INTEGER NCDMATCH
      REAL    ECAL_1NN
      REAL    ECAL_2NN,CALISO_2NN
      REAL    HFRACT,EFRACT_H1
      REAL    XY_IMPACT,RZ_IMPACT,T0FLOAT
      INTEGER IFW2
      INTEGER COSREJ_TRK,COSREJ_HIT
      REAL    XOCT
      REAL    DR_JET_NEAR_MU,ET_JET_NEAR_MU,DPHI_JET_MU,DETA_JET_MU
      REAL    DR_JET_NEAR_MU_TRY
      REAL    MU_BDL
      LOGICAL HITPLN_JUNK
      INTEGER QCD_PRESCALE,N_QCD
      REAL    MUJ_STAT(20),MUJ_SUMRY(20)
C
      INTEGER GZPMUO,GZPARH
      LOGICAL EZERR
      EXTERNAL GZPMUO,GZPARH,EZERR
C
      INTEGER I_JETS,MAX_JETS
      PARAMETER (MAX_JETS=20)
      REAL    JET_ET(0:MAX_JETS),JET_ETA_MAX
      INTEGER MULTI_JET_COUNT
C
      INTEGER MU_QUAD_MAX
      REAL    MU_PT_MIN
      INTEGER MU_IFW4_MAX_CF,MU_NCD_TRK_MIN_CF
      REAL    MU_ECAL_1NN_MIN_CF,MU_HFRACT_MIN_CF
      INTEGER MU_IFW4_MAX_EF,MU_NCD_TRK_MIN_EF
      REAL    MU_ECAL_1NN_MIN_EF,MU_HFRACT_MIN_EF
      REAL    MU_IMP_XY_MAX_CF,MU_IMP_RZ_MAX_CF,MU_T0FLOAT_MAX_CF
      REAL    MU_IMP_XY_MAX_EF,MU_IMP_RZ_MAX_EF,MU_T0FLOAT_MAX_EF
      REAL    MU_BDL_MIN
      INTEGER ICHOICE
      LOGICAL DO_XOCT_REJ
      INTEGER COSMIC_HIT_REJ_S,COSMIC_TRK_REJ_S
      INTEGER COSMIC_HIT_REJ_Q,COSMIC_TRK_REJ_Q
      INTEGER COSMIC_HIT_REJ_C,COSMIC_TRK_REJ_C
      LOGICAL PASS_MU_HITPLN_CUT_S,PASS_MU_HITPLN_CUT_C
      INTEGER JET_S_NUM,JET_Q_NUM,JET_C_NUM,JET_F_NUM
      REAL    JET_ETMIN_S,JET_ETMIN_Q,JET_ETMIN_C,JET_ETMIN_F
      REAL    ISO_JET_ET_MIN,ISO_JET_DR_MIN
C
      INTEGER LEMOBJ,J
      LOGICAL DO_EM_JET_REJECTION,IS_EM_OBJECT
      REAL    EM_THRESHOLD
C
      LOGICAL FIRST
      DATA FIRST /.TRUE./
      REAL TEMPLATE(5,4)
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
      DATA N_QCD/0/
      SAVE N_QCD,EM_THRESHOLD,DO_EM_JET_REJECTION,JET_ETA_MAX
C----------------------------------------------------------------------
      IF (FIRST) THEN
C
C   Get parameters from TOP_MUJET.RCP
        CALL INRCP('TOP_MUJET_RCP',IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('TOP_MUJET','TOP_MUJET_RCP',
     &        'ERROR READING TOP_MUJET RCP FILE','W')
          STOP 'TOP_MUJET_RCP not found'
        ELSE
          CALL EZPICK('TOP_MUJET_RCP')
C parameters for SIGNAL, COSMIC, and QCD
          IF (IER.EQ.0) CALL EZGET('MU_QUAD_MAX',MU_QUAD_MAX, IER)
          IF (IER.EQ.0) CALL EZGET('MU_PT_MIN',MU_PT_MIN, IER)
          IF (IER.EQ.0) CALL EZGET('DO_XOCT_REJ',DO_XOCT_REJ, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IFW4_MAX_CF',MU_IFW4_MAX_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IFW4_MAX_EF',MU_IFW4_MAX_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_NCD_TRK_MIN_CF',
     &                              MU_NCD_TRK_MIN_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_NCD_TRK_MIN_EF',
     &                              MU_NCD_TRK_MIN_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_ECAL_1NN_MIN_CF',
     &                              MU_ECAL_1NN_MIN_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_ECAL_1NN_MIN_EF',
     &                              MU_ECAL_1NN_MIN_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_HFRACT_MIN_CF',
     &                              MU_HFRACT_MIN_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_HFRACT_MIN_EF',
     &                              MU_HFRACT_MIN_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IMP_XY_MAX_CF',
     &                              MU_IMP_XY_MAX_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IMP_XY_MAX_EF',
     &                              MU_IMP_XY_MAX_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IMP_RZ_MAX_CF',
     &                              MU_IMP_RZ_MAX_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_IMP_RZ_MAX_EF',
     &                              MU_IMP_RZ_MAX_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_T0FLOAT_MAX_CF',
     &                              MU_T0FLOAT_MAX_CF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_T0FLOAT_MAX_EF',
     &                              MU_T0FLOAT_MAX_EF, IER)
          IF (IER.EQ.0) CALL EZGET('MU_BDL_MIN',MU_BDL_MIN, IER)
          IF (IER.EQ.0) CALL EZGET('JET_CONE_ICHOICE',ICHOICE,
     &      IER)
C cuts for signal
          IF (IER.EQ.0) CALL EZGET('COSMIC_HIT_REJ_S',
     &                              COSMIC_HIT_REJ_S, IER)
          IF (IER.EQ.0) CALL EZGET('COSMIC_TRK_REJ_S',
     &                              COSMIC_TRK_REJ_S, IER)
          IF (IER.EQ.0) CALL EZGET('PASS_MU_HITPLN_CUT_S',
     &                              PASS_MU_HITPLN_CUT_S, IER)
          IF (IER.EQ.0) CALL EZGET('JET_ETA_MAX',JET_ETA_MAX,IER)
          IF (IER.EQ.0) CALL EZGET('JET_ETMIN_S',JET_ETMIN_S,IER)
          IF (IER.EQ.0) CALL EZGET('JET_S_NUM',JET_S_NUM,IER)
          IF (IER.EQ.0) CALL EZGET('ISO_JET_ET_MIN',ISO_JET_ET_MIN,IER)
          IF (IER.EQ.0) CALL EZGET('ISO_JET_DR_MIN',ISO_JET_DR_MIN,IER)
C cuts for QCD
          IF (IER.EQ.0) CALL EZGET('COSMIC_HIT_REJ_Q',
     &                              COSMIC_HIT_REJ_Q, IER)
          IF (IER.EQ.0) CALL EZGET('COSMIC_TRK_REJ_Q',
     &                              COSMIC_TRK_REJ_Q, IER)
          IF (IER.EQ.0) CALL EZGET('JET_ETMIN_Q',JET_ETMIN_Q,IER)
          IF (IER.EQ.0) CALL EZGET('JET_Q_NUM',JET_Q_NUM,IER)
          IF (IER.EQ.0) CALL EZGET('QCD_PRESCALE',QCD_PRESCALE,IER)
C cuts for COSMIC
          IF (IER.EQ.0) CALL EZGET('COSMIC_HIT_REJ_C',
     &                              COSMIC_HIT_REJ_C, IER)
          IF (IER.EQ.0) CALL EZGET('COSMIC_TRK_REJ_C',
     &                              COSMIC_TRK_REJ_C, IER)
          IF (IER.EQ.0) CALL EZGET('PASS_MU_HITPLN_CUT_C',
     &                              PASS_MU_HITPLN_CUT_C,IER)
          IF (IER.EQ.0) CALL EZGET('JET_ETMIN_C',JET_ETMIN_C,IER)
          IF (IER.EQ.0) CALL EZGET('JET_C_NUM',JET_C_NUM,IER)
C- cuts for excluding em jets from counting...
          IF (IER.EQ.0) CALL EZGET('IGNORE_EM_JETS',
     &      DO_EM_JET_REJECTION,IER)
          IF (IER.EQ.0) CALL EZGET('EM_THRESHOLD',EM_THRESHOLD,IER)
C parameters for FAKE ONLY
           IF (IER.EQ.0) CALL EZGET('JET_ETMIN_F',JET_ETMIN_F,IER)
          IF (IER.EQ.0) CALL EZGET('JET_F_NUM',JET_F_NUM,IER)
C done with defining all the selection cuts.
          IF (IER.NE.0) THEN
            CALL ERRMSG('TOP_MUJET','TOP_MUJET_RCP',
     &        'ERROR GETTING TOP_MUJET RCP VALUES','W')
          ENDIF
          CALL EZRSET
          CALL VZERO(MUJ_STAT,20)
        ENDIF
        FIRST = .FALSE.
      ENDIF
C----------------------------------------------------------------------
      PASSED_ALL = .FALSE.
      PASSED_SIGNAL = .FALSE.
      PASSED_COSMIC = .FALSE.
      PASSED_QCD = .FALSE.
      PASSED_MU = .FALSE.
      PASSED_FAKE = .FALSE.
      MUJ_STAT(1) = MUJ_STAT(1) + 1.
C
C set jet algorithm
C
      IF(ICHOICE.EQ.1) CALL SET_CAPH('CONE_JET',TEMPLATE(1,1),IER)
      IF(ICHOICE.EQ.2) CALL SET_CAPH('CONE_JET',TEMPLATE(1,2),IER)
      IF(ICHOICE.EQ.3) CALL SET_CAPH('CONE_JET',TEMPLATE(1,3),IER)
      IF(ICHOICE.EQ.4) CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
C
C Sort jets by ET (decreasing in ET)
C Record ordered list of highest ET jets (MAX_JETS)
C
      MULTI_JET_COUNT = 0
      DO I_JETS = 0 ,MAX_JETS
        JET_ET(I_JETS) = 0.
      ENDDO
      I_JETS = 0
      LJETS=GZJETS()
      IF(LJETS.NE.0) THEN
        CALL ZSORT(IXCOM,LJETS,6)
        LJETS=GZJETS()
        CALL ZTOPSY(IXCOM,LJETS)
        LJETS=GZJETS()
        DO WHILE (LJETS.GT.0 .AND. I_JETS.LE.(MAX_JETS-1))
          IF( ABS(Q(LJETS+9)).LE.JET_ETA_MAX ) THEN
            I_JETS = I_JETS + 1
            JET_ET(I_JETS) = SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            IF(JET_ET(I_JETS).GE.JET_ETMIN_F)THEN
              MULTI_JET_COUNT = MULTI_JET_COUNT + 1
            ENDIF
          ENDIF
          LJETS=LQ(LJETS)          ! pointer to next jet
        ENDDO
      ENDIF
      IF (MULTI_JET_COUNT.GE.JET_F_NUM)THEN
        PASSED_FAKE = .TRUE.
        MUJ_STAT(4) = MUJ_STAT(4) + 1.
      ENDIF
C
C check PARH bank
C
      LPARH=GZPARH()
      NPMUO = 0
      IF (LPARH.GT.0) THEN
        NPMUO=IQ(LPARH+2)
      ELSE
        GOTO 900                   !Exit immediately if no PARH bank
      ENDIF
      IF (NPMUO.LE.0)THEN
        GOTO 900                   !Exit immediately if no PMUO banks
      ENDIF
C
C find number of good muons in the event
C
      MU_QUAD = -10.0
      MU_PT = 0.
      MU_ETA = 0.
      MU_PHI = 0.
      MU_P   = 0.
      IFW4 = 99
      NCDMATCH = 0
      ECAL_1NN = -10.
c
      ECAL_2NN = -10.
      CALISO_2NN = -10.
c
      HFRACT = -10.0
      EFRACT_H1 = -10.0
c
      XY_IMPACT = -99999.
      RZ_IMPACT = -99999.
      T0FLOAT = -99999.
c
      IFW2 = 99
      COSREJ_TRK = -10
      COSREJ_HIT = -10
      XOCT   = -10.
c
      MU_BDL = -99999.
      LPMUO=GZPMUO(0)
C
C ****  Loop over all muons; check PT and eta for quick jump out
C
      DO WHILE(LPMUO.GT.0)
        MU_QUAD = IQ(LPMUO+7)
        MU_PT = Q(LPMUO+14)
        IF (MU_QUAD.GT.MU_QUAD_MAX .OR.
     >      MU_PT.LT.MU_PT_MIN) GOTO 100
C PMUO
        MU_ETA = Q(LPMUO+16)
        MU_PHI = Q(LPMUO+17)
        MU_P   = Q(LPMUO+13)
        IFW4 = IQ(LPMUO+9)
        NCDMATCH = IQ(LPMUO+6)
        ECAL_1NN = Q(LPMUO+84)
        ECAL_2NN = Q(LPMUO+34)
        CALISO_2NN = Q(LPMUO+30)
        XY_IMPACT = Q(LPMUO+57)
        RZ_IMPACT = Q(LPMUO+56)
        T0FLOAT = Q(LPMUO+24)
c add MTC stuff in.
        HFRACT = Q(LPMUO+94)
        EFRACT_H1 = Q(LPMUO+98)
c
        IFW2 = IQ(LPMUO+44)     ! If BTEST .TRUE., muon failed cut
        IF (BTEST(IFW2,6)) THEN
          COSREJ_TRK = -1
        ELSE
          COSREJ_TRK = 1
        ENDIF
        IF (BTEST(IFW2,7)) THEN
          COSREJ_HIT = -1
        ELSE
          COSREJ_HIT = 1
        ENDIF
        IF (BTEST(IFW2,8)) THEN
          XOCT = -1.0
        ELSE
          XOCT = 1.0
        ENDIF
C MUOT stuff
        NS = IQ(LPMUO-2)
        LMUOT = LQ(LPMUO-NS-1)
        MU_BDL = Q(LMUOT+22)
        CALL MU_HITPLN_CUT(LMUOT,HITPLN_JUNK) !HITPLN_JUNK TRUE if bad
C
C Get ET & delta R OF jet nearest to muon
C
        DR_JET_NEAR_MU = 9999.
        ET_JET_NEAR_MU = -9999.
        LJETS = GZJETS()
        DO WHILE (LJETS.GT.0)
C  Don't look at jet if it's a PELC/PPHO...
          IS_EM_OBJECT = .FALSE.
          IF( DO_EM_JET_REJECTION ) THEN
            DO J=1,4
              LEMOBJ = LQ(LJETS-J-2)
              IF( LEMOBJ.GT.0 ) THEN
                IF( Q(LEMOBJ+7).GT.EM_THRESHOLD ) IS_EM_OBJECT = .TRUE.
              ENDIF
            ENDDO
          ENDIF
C
          IF( .NOT.IS_EM_OBJECT ) THEN
            DPHI_JET_MU = ABS(Q(LJETS+8)-MU_PHI)
            IF(DPHI_JET_MU.GT.PI) DPHI_JET_MU = TWOPI - DPHI_JET_MU
            DETA_JET_MU = ABS(Q(LJETS+9)-MU_ETA)
            DR_JET_NEAR_MU_TRY = SQRT(DETA_JET_MU**2+DPHI_JET_MU**2)
            IF (DR_JET_NEAR_MU_TRY.LT.DR_JET_NEAR_MU) THEN
              DR_JET_NEAR_MU = DR_JET_NEAR_MU_TRY
              ET_JET_NEAR_MU = SQRT(Q(LJETS+2)**2+Q(LJETS+3)**2)
            ENDIF
          ENDIF
          LJETS=LQ(LJETS)          ! pointer to next jet
        ENDDO
C
C ****  Apply muon cuts common to all cuts in CF and EF seperately:
C
        IF( ((MU_QUAD.LE.4                                   .AND.
     &        IFW4.LE.MU_IFW4_MAX_CF                         .AND.
     &        ( NCDMATCH.GE.MU_NCD_TRK_MIN_CF  .OR.
     &          ECAL_1NN.GE.MU_ECAL_1NN_MIN_CF .OR.
     &          HFRACT.GE.MU_HFRACT_MIN_CF )                 .AND.
c made some changes here
     &        ABS(XY_IMPACT).LE.MU_IMP_XY_MAX_CF             .AND.
     &        ABS(RZ_IMPACT).LE.MU_IMP_RZ_MAX_CF             .AND.
     &        ABS(T0FLOAT).LE.MU_T0FLOAT_MAX_CF)                .OR.
c
     &         (MU_QUAD.GT.4                                  .AND.
     &         IFW4.LE.MU_IFW4_MAX_EF                        .AND.
     &         ( NCDMATCH.GE.MU_NCD_TRK_MIN_EF .OR.
     &           ECAL_1NN.GE.MU_ECAL_1NN_MIN_EF .OR.
     &           HFRACT.GE.MU_HFRACT_MIN_EF )                .AND.
c made some changes here
     &         ABS(XY_IMPACT).LE.MU_IMP_XY_MAX_EF            .AND.
     &         ABS(RZ_IMPACT).LE.MU_IMP_RZ_MAX_EF            .AND.
     &         ABS(T0FLOAT).LE.MU_T0FLOAT_MAX_EF))           .AND.
c
     &       ( MU_BDL.GE.MU_BDL_MIN    .or.
     &         ABS(MU_ETA).LE.0.7 )                          .AND.
     &       ((.NOT.DO_XOCT_REJ).OR.(XOCT.GT.0)) ) THEN
C
C ****  Apply signal cuts
C
 777      IF( (COSMIC_HIT_REJ_S.EQ.0.OR.
     1         COSMIC_HIT_REJ_S.EQ.COSREJ_HIT)                  .AND.
     2        (COSMIC_TRK_REJ_S.EQ.0.OR.
     3         COSMIC_TRK_REJ_S.EQ.COSREJ_TRK)                  .AND.
c jet Et and multiplicity
     4        (JET_S_NUM.LE.0 .OR.
     5         JET_ET(JET_S_NUM).GE.JET_ETMIN_S)                .AND.
c isolation
     6        (DR_JET_NEAR_MU.GE.ISO_JET_DR_MIN.OR.
     7         ET_JET_NEAR_MU.LT.ISO_JET_ET_MIN)                .AND.
c this is something else:
     8        (.NOT.(PASS_MU_HITPLN_CUT_S).OR.
     9         PASS_MU_HITPLN_CUT_S.AND.(.NOT.HITPLN_JUNK)) )  THEN
c ok, this event passed all our signal cuts, say YES
            PASSED_SIGNAL = .TRUE.
            MUJ_STAT(5) = MUJ_STAT(5) + 1
          ENDIF
C
C ****  Apply QCD cuts
C
 888      IF( (COSMIC_HIT_REJ_Q.EQ.0.OR.
     1         COSMIC_HIT_REJ_Q.EQ.COSREJ_HIT)                  .AND.
     2        (COSMIC_TRK_REJ_Q.EQ.0.OR.
     3         COSMIC_TRK_REJ_Q.EQ.COSREJ_TRK) )  THEN
c do something different here:
            IF( JET_Q_NUM.LE.0 .OR.
     4          JET_ET(JET_Q_NUM).GE.JET_ETMIN_Q )  THEN
c YES, we want to keep this event as our QCD bkg
              PASSED_QCD = .TRUE.
              MUJ_STAT(7) = MUJ_STAT(7) + 1
            ELSE IF( JET_ET(JET_Q_NUM).LE.JET_ETMIN_Q           .AND.
     5               JET_ET(JET_S_NUM).GE.JET_ETMIN_Q           .AND.
     6               DR_JET_NEAR_MU.LT.ISO_JET_DR_MIN           .AND.
     7               ET_JET_NEAR_MU.GE.ISO_JET_ET_MIN )  THEN
c prescale this kind of events since the high rate for QCD events
C           x_random = RNDM()
C           IF( x_random.LE.0.20000 ) THEN
c prescaled as 1/5.
              N_QCD = N_QCD + 1
              IF( MOD(N_QCD,QCD_PRESCALE).EQ.0 ) THEN
                PASSED_QCD = .TRUE.
                MUJ_STAT(7) = MUJ_STAT(7) + 1
              ENDIF
            ENDIF    !end of different jet_multiplicity treatment
          ENDIF    !end of QCD selection
C
C ****  Apply cosmic cuts
C
          IF( ((COSMIC_HIT_REJ_C.EQ.0.OR.
     1          COSMIC_HIT_REJ_C.EQ.COSREJ_HIT).OR.
     2         (COSMIC_TRK_REJ_C.EQ.0.OR.
     3          COSMIC_TRK_REJ_C.EQ.COSREJ_TRK))                .AND.
     5        (JET_C_NUM.LE.0 .OR.
     6         JET_ET(JET_C_NUM).GE.JET_ETMIN_C)                .AND.
     7        (.NOT.(PASS_MU_HITPLN_CUT_C).OR.
     8         PASS_MU_HITPLN_CUT_C.AND.(.NOT.HITPLN_JUNK)) )  THEN
            PASSED_COSMIC = .TRUE.
            MUJ_STAT(6) = MUJ_STAT(6) + 1
          ENDIF
c now all the selections for signal/QCD/cosmic are done.
        ENDIF   !End of common muon cuts
        IF (PASSED_SIGNAL .OR. PASSED_COSMIC .OR. PASSED_QCD )THEN
          PASSED_MU = .TRUE.
          GOTO 900
        ENDIF
  100   LPMUO=LQ(LPMUO)
      ENDDO
  900 IF (PASSED_FAKE .OR. PASSED_MU )THEN
        PASSED_ALL = .TRUE.
        MUJ_STAT(2) = MUJ_STAT(2) + 1.
      ENDIF
      CALL RESET_CAPH
      TOP_MUJET = PASSED_ALL
C
  999 RETURN
C----------------------------------------------------------------------
      ENTRY TOP_MUJET_EOJ(MUJ_SUMRY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End of job statistics for TOP->MUJET filter
C-
C- MUJ_SUMRY  1 = total number of events on which filter was run
C-            2 = total number of events passed
C-            3 = fraction of events accepted (%)
C-            4 = number of accepted events for FAKE
C-            5 = number of accepted events for SIGNAL
C-            6 = number of accepted events for COSMIC
C-            7 = number of accepted events for QCD
C-            8 = fraction of accepted events for FAKE (%)
C-            9 = fraction of accepted events for SIGNAL (%)
C-            10= fraction of accepted events for COSMIC (%)
C-            11= fraction of accepted events for QCD (%)
C-
C-
C----------------------------------------------------------------------
      TOP_MUJET_EOJ=.TRUE.
      CALL UCOPY(MUJ_STAT,MUJ_SUMRY,20)
      IF(MUJ_STAT(1).GT.0.) MUJ_SUMRY(3)=MUJ_STAT(2)/MUJ_STAT(1)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(8)=MUJ_STAT(4)/MUJ_STAT(2)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(9)=MUJ_STAT(5)/MUJ_STAT(2)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(10)=MUJ_STAT(6)/MUJ_STAT(2)*100.
      IF(MUJ_STAT(2).GT.0.) MUJ_SUMRY(11)=MUJ_STAT(7)/MUJ_STAT(2)*100.
C
      RETURN
      END

