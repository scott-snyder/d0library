      FUNCTION NP_MSP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select at least 2 high P muons as candidates for
C-                         Massive Stable Particles
C-
C-   Returned value  : .TRUE. if there are 2 or more candidates
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  16-FEB-1993   Du\v{s}an Ne\v{s}i\'{c}
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL COPLANAR, FIRST, LCOSMIC, NP_MSP, NP_MSP_EOJ
C
      INTEGER GZPMUO, I, IER, IOS, J, LPMUO, NGMUO, NMUON, NUM_MUO,
     &        NUM_MUO_MAX, NUM_MUO_MIN, MSTAT, GOOD_MUON1 
C
      REAL    ANG, CONV, DEDX_MIN, CALEN_MIN, LEAK_SUM, 
     &        P_GMUO(4,20), RSUMM(20), RSUMMARY(20), SIGEN_SUM,
     &        THETA1_2MAX, THETA1_2MIN, MUON_ETA, MUON_PT,
     &        MUON_PT_CUT, MUON_ETA_CUT
C
      DATA FIRST/.TRUE./
      DATA CONV/57.2957796/
      SAVE FIRST
C----------------------------------------------------------------------
C
      IF ( FIRST ) THEN
        FIRST=.FALSE.
C
        DO I = 1, 20
          RSUMMARY(I) = 0.0
        ENDDO
        CALL INRCP('NP_MSP_RCP', IER)
        IF(IER.EQ.0) THEN
          CALL EZPICK('NP_MSP_RCP')
          CALL EZGET('THETA1_2MIN', THETA1_2MIN, IER)
          CALL EZGET('THETA1_2MAX', THETA1_2MAX, IER)
          CALL EZGET('DEDX_MIN', DEDX_MIN, IER)
          CALL EZGET('LEAK_SUM', LEAK_SUM, IER)
          CALL EZGET('SIGEN_SUM', SIGEN_SUM, IER)
          CALL EZGET('CALEN_MIN', CALEN_MIN, IER)
          CALL EZGET_i('NUM_MUO_MIN', NUM_MUO_MIN, IER)
          CALL EZGET_i('NUM_MUO_MAX', NUM_MUO_MAX, IER)
          CALL EZGET('MUON_PT_CUT',MUON_PT_CUT,IER)
          CALL EZGET('MUON_ETA_CUT',MUON_ETA_CUT,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG ('No NP_MSP_RCP', 'NP_MSP_EVENT',
     &        'Could not find NP_MSP_RCP', 'F')
          GOTO 999
        ENDIF                           ! if ier .eq. 0
      ENDIF                             ! if first
C
C ****  Assume event will not pass
C
      NP_MSP = .FALSE.
      IF(LHEAD.EQ.0) GOTO 999                ! no filtering
C
      IOS=MOD(IQ(LHEAD+1), 1000)
      IF(IOS.LT.5) THEN
        CALL ERRMSG ('IQ(LHEAD+1) WRONG', 'NP_MSP_EVENT',
     &        'Not a real or Monte Carlo event', 'F')
        GOTO 999     ! not an event record
      ENDIF
C
C ****  Order PMUO banks; first in increasing order of Pt then in decreasing
C ****  order in Pt
C
      LPMUO=GZPMUO(0)
      CALL ZSORT(IXCOM,LPMUO,14)
      LPMUO=GZPMUO(0)
      CALL ZTOPSY(IXCOM,LPMUO)
      LPMUO=GZPMUO(0)
C
C ****  Are there at least NUM_MUO_MIN muons? More than NUM_MUO_MAX?
C
      CALL GTPMUO_TOTAL(NUM_MUO, IER)
      IF (IER.NE.0 .OR. NUM_MUO.LT.NUM_MUO_MIN .OR. 
     &    NUM_MUO.GE.NUM_MUO_MAX) GO TO 999
      RSUMMARY(1) = RSUMMARY(1) + 1.0
C
C **** Emulating wyatt's program  
C
      NMUON =0
      NGMUO=0
      LPMUO=GZPMUO(0)
      DO WHILE(LPMUO.NE.0)
        NMUON=NMUON+1                     ! NUMBER OF MUONS
        MUON_PT=Q(LPMUO+14)
        MUON_ETA=Q(LPMUO+16)
        CALL MSP_MUON_SELECT(LPMUO,MSTAT,GOOD_MUON1)
        IF (GOOD_MUON1.GT.0.AND.MUON_PT.GT.MUON_PT_CUT.AND.
     &ABS(MUON_ETA).LT.MUON_ETA_CUT)  THEN
          NGMUO = NGMUO + 1
          P_GMUO(1,NGMUO) = Q(LPMUO+10) !PX
          P_GMUO(2,NGMUO) = Q(LPMUO+11) !PY
          P_GMUO(3,NGMUO) = Q(LPMUO+12) !PZ
          P_GMUO(4,NGMUO) = Q(LPMUO+13) !P
        ENDIF
        LPMUO = LQ(LPMUO)
      ENDDO
      IF (NGMUO.GT.1) RSUMMARY(2) = RSUMMARY(2) + 1.0
C
C ****  Check for back-to-back muons, subtract them from list
C
      LCOSMIC=.FALSE.
      COPLANAR = .TRUE.
      IF(NGMUO.GT.1) THEN
        DO I=1,NGMUO-1
          DO J=I+1,NGMUO
            IF ( ABS((P_GMUO(4,I)*P_GMUO(4,J))).LE.0.000001) THEN
              GOTO 999
            ENDIF
            ANG=(P_GMUO(1,I)*P_GMUO(1,J)+P_GMUO(2,I)*P_GMUO(2,J)+
     &        P_GMUO(3,I)*P_GMUO(3,J))/(P_GMUO(4,I)*P_GMUO(4,J))
            IF(ANG.LT.-1.)ANG=-1.
            IF(ANG.GT.1.)ANG=1.
            IF(ACOS(ANG)*CONV.GT.THETA1_2MAX)LCOSMIC=.TRUE.
            IF(ACOS(ANG)*CONV.LT.THETA1_2MIN)COPLANAR=.FALSE.
          ENDDO
        ENDDO
      ENDIF
C
C ****  Subtract the back-to-back tracks
C
      IF(LCOSMIC.OR.(.NOT.COPLANAR)) NGMUO=NGMUO-2
      IF (NGMUO.GE.2) NP_MSP = .TRUE.
      IF (NP_MSP) RSUMMARY(3) = RSUMMARY(3) + 1.0
C
  999 RETURN
C8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<8<88<8<8<8<8<8<8<8<
      ENTRY NP_MSP_EOJ(RSUMM)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : End-of-job summary for NP_MSP filter
C-
C-   Inputs  : none
C-   Outputs : RSUMMARY   [R(20)]  array of 20 real numbers
C-   Controls: none
C-
C-   Created  16-FEB-1993   Du\v{s}an Ne\v{s}i\'{c}
C-
C----------------------------------------------------------------------
      NP_MSP_EOJ = .TRUE.
      DO I=1, 20
        RSUMM(I) = RSUMMARY(I)
      ENDDO
      RETURN
      END
