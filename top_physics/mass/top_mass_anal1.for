      SUBROUTINE TOP_MASS_ANAL1
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Analyze mass banks
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-SEP-1993   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$INC:KINEQ.INC'
      INCLUDE 'D0$INC:TOP_SOLNS.INC'
      INCLUDE 'D0$INC:TOP_SOLNSE.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      INTEGER IER
      LOGICAL first
      SAVE first
      DATA first / .true. /
C
      INTEGER LMASS,GZMASS
      EQUIVALENCE (LMASS,CSTLNK(LNKMX))
C
      INTEGER CM
      PARAMETER( CM = 7 )   !Maximum number of COMBNUM'S
      REAL    LIKE(2,2,CM),MASS(2,2,CM),LOCL(2,2,CM),HICL(2,2,CM),
     &  TURN_ON(2,CM),BTFL(CM),NTAGS(CM),DIF_R(2,CM),DIF_ET(2,CM),
     &  NCONF(CM)
      INTEGER NAME(2,CM)
      INTEGER COM_NUM
      REAL    MAX_LIKE(2),MASS_MAX(2)
C
      INTEGER CMB_MAX(2),COM_MAX(2)
      INTEGER IPT,ICOMB,IRR,STATUS
      REAL    WT
      REAL    BTFL_MAX
      INTEGER COM_NUM_MAX
      INTEGER I,IP
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('TOP_MASS_RCP')
        CALL DO_HBOOK_OPEN('HBOOK_OPEN',STATUS)
        CALL DHDIR('TOP_MASS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('TOP_MASS','TOP_MASS_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL DO_HBOOK('MASS_HISTS')
        CALL EZRSET
      ENDIF
      CALL DHDIR_DECLARE_FILE('DILEPTON')
      CALL DHDIR('TOP_MASS_RCP','HBOOK_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG('TOP_MASS','TOP_MASS_ANAL',
     &      ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      MAX_LIKE(1) = 0.
      MAX_LIKE(2) = 0.
      MASS_MAX(1) = 0.
      MASS_MAX(2) = 0.
C
      LMASS = GZMASS()
      IPT = 0
      DO WHILE (LMASS.NE.0)
        COM_NUM = Q(LMASS+34)
        IPT = 4
        DO ICOMB = 1 , 2
          DO IRR = 1 , 2
            MASS(IRR,ICOMB,COM_NUM) = Q(LMASS+IPT)
            LIKE(IRR,ICOMB,COM_NUM) = Q(LMASS+IPT+1)
            LOCL(IRR,ICOMB,COM_NUM) = Q(LMASS+IPT+2)
            HICL(IRR,ICOMB,COM_NUM) = Q(LMASS+IPT+3)
            IPT = IPT + 4
            IF ( COM_NUM.NE.7 ) THEN
              IF ( LIKE(IRR,ICOMB,COM_NUM).GT.MAX_LIKE(IRR) ) THEN
                MAX_LIKE(IRR) = LIKE(IRR,ICOMB,COM_NUM)
                MASS_MAX(IRR) = MASS(IRR,ICOMB,COM_NUM)
                CMB_MAX(IRR) = ICOMB
                COM_MAX(IRR) = COM_NUM
              ENDIF
            ELSE
C IF HERE ISAJET RAW SOLUTIONS
            ENDIF
          ENDDO
C
          TURN_ON(ICOMB,COM_NUM) = Q(LMASS+IPT)
          IPT = IPT + 1
C
        ENDDO
C
        BTFL(COM_NUM) = Q(LMASS+35)
        NTAGS(COM_NUM) = Q(LMASS+36)
        NAME(1,COM_NUM) = IQ(LMASS+37)
        DIF_R(1,COM_NUM) = Q(LMASS+38)
        DIF_ET(1,COM_NUM) = Q(LMASS+39)
C
        NAME(2,COM_NUM) = IQ(LMASS+40)
        DIF_R(2,COM_NUM) = Q(LMASS+41)
        DIF_ET(2,COM_NUM) = Q(LMASS+42)
        NCONF(COM_NUM) = Q(LMASS+43)
        LMASS = LQ(LMASS)
      ENDDO
C
      IF ( IPT.EQ.0 ) THEN
C
C NO SOLUTIONS
C
        GO TO 999
      ENDIF
C

      IF ( MASS_MAX(1).GT.0.0 ) THEN
        CALL DO_HF1(801,MASS_MAX(1),1.0)
        CALL DO_HF1(803,-ALOG10(MAX_LIKE(1)),1.0)
        WT = 7.0 + ALOG10(MAX_LIKE(1))
        IF(WT.GT.0.0)CALL DO_HF1(805,MASS_MAX(1),WT)
        IRR = 1
        COM_NUM_MAX = COM_MAX(IRR)
        BTFL_MAX = BTFL(COM_NUM_MAX)
        IF(BTFL_MAX.EQ.CMB_MAX(IRR))THEN
C CORRECT COMBINATION
          CALL DO_HF1(811,MASS_MAX(IRR),1.0)
          CALL DO_HF1(813,-ALOG10(MAX_LIKE(IRR)),1.0)
          WT = 7.0 + ALOG10(MAX_LIKE(IRR))
          IF(WT.GT.0.0)CALL DO_HF1(815,MASS_MAX(1),WT)
        ENDIF
        COM_NUM_MAX = COM_MAX(1)
        CALL DO_HF1(851,TURN_ON(CMB_MAX(1),COM_NUM_MAX),1.0)
      ENDIF
C
      IF ( MASS_MAX(2).GT.0.0 ) THEN
        CALL DO_HF1(802,MASS_MAX(2),1.0)
        CALL DO_HF1(804,-ALOG10(MAX_LIKE(2)),1.0)
        WT = 2.0 + ALOG10(MAX_LIKE(2))
        IF(WT.GT.0.0)CALL DO_HF1(806,MASS_MAX(1),WT)
        IRR = 2
        COM_NUM_MAX = COM_MAX(IRR)
        BTFL_MAX = BTFL(COM_NUM_MAX)
        IF(BTFL_MAX.EQ.CMB_MAX(IRR))THEN
C CORRECT COMBINATION
          CALL DO_HF1(812,MASS_MAX(IRR),1.0)
          CALL DO_HF1(814,-ALOG10(MAX_LIKE(IRR)),1.0)
          WT = 2.0 + ALOG10(MAX_LIKE(IRR))
          IF(WT.GT.0.0)CALL DO_HF1(816,MASS_MAX(1),WT)
        ENDIF
        COM_NUM_MAX = COM_MAX(2)
        CALL DO_HF1(852,TURN_ON(CMB_MAX(2),COM_NUM_MAX),1.0)
      ENDIF
C
      CALL DO_HF2(820,MASS_MAX(1),MASS_MAX(2),1.0)
C
      DO I = 1 , 3
        CALL DO_HF1(830+I,TURN_ON(1,I),1.0)
        CALL DO_HF1(840+I,TURN_ON(2,I),1.0)
      ENDDO
C
      DO IRR = 1 , 2
        IP = (IRR-1)*10
        DO ICMB = 1 , 2
          DO ICOMB = 1 , 3
            IP = IP+1
            CALL DO_HF1(860+IP,MASS(IRR,ICMB,ICOMB),1.0)
          ENDDO
        ENDDO
      ENDDO
C
C ****  PLOT ISAJET RAW INFORMATION
C
      IF ( LIKE(1,1,7).GT.0.0 ) THEN
        CALL DO_HF1(901,MASS(1,1,7),1.0)
        CALL DO_HF1(903,-ALOG10(LIKE(1,1,7)),1.0)
      ENDIF
      IF ( LIKE(2,1,7).GT.0.0 ) THEN
        CALL DO_HF1(902,MASS(2,1,7),1.0)
        CALL DO_HF1(904,-ALOG10(LIKE(2,1,7)),1.0)
      ENDIF
C
  999 RETURN
      END
