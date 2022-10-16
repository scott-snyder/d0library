      LOGICAL FUNCTION TOP_TOP_EJET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter e+jets events for TOP stream using the
C-          following criteria
C-          - Select EM filters
C-          - 1 PELC with Pt > ELEC_ETCUT
C-                        DEta < ELEC_ETACUT
C-                        Isol < ELEC_ISOLCUT
C-          - Njets >= NJET_MIN  (cone size = .5)
C-                 Etjet > JET_ETCUT
C-                 Deta  < JET_ETACUT  
C-          - Missing Et MET(PNUT3 OR PNUT2/PNUT1) >= MET_CUT      
C-
C-   Returned value  : .TRUE. for events that pass the criteria
C-   Controls: TOP_TOP_EJET_RCP
C-
C-   Created  19-JUL-1993   Pushpa C. Bhat
C-   Updated  18-AUG-1993   Pushpa C. Bhat  Write an error message when
C-                                            ZVERT=0
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKA.INC'
      INTEGER GZHEAD,GZCAPH,GZJETS,GZVERH,GZPELC,GZPNUT,GZVERT,GZPPHO
      INTEGER LCAPH,LJETS,LVERH,LVERT,LPELC,LHMTE,LJNEP,LCACL,LPNUT3,
     &  LPNUT, LPPHO  
      INTEGER JET_ALGO,NJET_MIN,NJET,NELEC,I,NPHOT
      INTEGER II,III,JJ,IER,ERR,RUNNO,EVONUM,STATUS,VERT_STAT,JBIT,ILEN
      EXTERNAL RUNNO,EVONUM,JBIT
      INTEGER NTRIGON,NFILTON,NFSTRING_REQ,LEN1
      INTEGER TBIT_ON(32),FBIT_ON(128)
      INTEGER EJET_STAT(10),LEPPHO_PNTR
      REAL    ELEC_ETCUT,ELEC_ETACUT,ELEC_ISOLCUT
      REAL    JET_ETCUT,JET_ETACUT
      REAL    MET_CUT
      REAL    TEMPLATE(5,4)
      REAL    ZVERT,TOP_TOP_EJET_SUM(20)
      REAL    ET,ETA,ISOL,MET,THETA
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 TSTRING_REQ(32),FSTRING_REQ(128)
      CHARACTER*32 SEARCH_STRING
      LOGICAL FIRST,FLAG_ELEC,FLAG_JETS,FLAG_MET,PASS_L2
      LOGICAL TOP_TOP_EJET_EOJ,SELECT_GAMMA
      LOGICAL MATCH_WILD,SELECT_EM_FILTERS,PASS,EXPRESS_STREAMING
      EXTERNAL MATCH_WILD
      DATA FIRST /.TRUE./
      SAVE FIRST,EJET_STAT
      DATA TEMPLATE/
     &  1.,6.,0.7,0.,0.,      ! CONE R=0.7
     &  1.,6.,0.5,0.,0.,      ! CONE R=0.5
     &  1.,6.,0.3,0.,0.,      ! CONE R=0.3
     &  2.,7.,2.,8.,2./       ! NN 2x2
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_TOP_EJET_RCP',IER)
        IF (IER.EQ.0)THEN
          CALL EZPICK('TOP_TOP_EJET_RCP')
          CALL EZGET_l('SELECT_EM_FILTERS',SELECT_EM_FILTERS,IER)
          IF(SELECT_EM_FILTERS)
     &      CALL EZ_GET_CHARS('EM_FILTNAMES',NFSTRING_REQ,FSTRING_REQ,
     &        STATUS)
          CALL EZGET_i('JET_ALGO',JET_ALGO,IER)
          CALL EZGET('ELEC_ETCUT',ELEC_ETCUT,IER)
          CALL EZGET('ELEC_ETACUT',ELEC_ETACUT,IER)
          CALL EZGET('ELEC_ISOLCUT',ELEC_ISOLCUT,IER)
          CALL EZGET_i('NJET_MIN',NJET_MIN,IER)
          CALL EZGET('JET_ETCUT',JET_ETCUT,IER)
          CALL EZGET('JET_ETACUT',JET_ETACUT,IER)
          CALL EZGET('MET_CUT',MET_CUT,IER)
          CALL EZGET_l('SELECT_GAMMA',SELECT_GAMMA,IER)
          CALL EZGET_l('EXPRESS_STREAMING',EXPRESS_STREAMING,IER)
          CALL EZRSET
        ELSE
          CALL ERRMSG('TOP_TOP_EJET','TOP_TOP_EJET_RCP',
     &      'ERROR READING TOP_TOP_EJET RCP FILE','F')
        ENDIF
          CALL VZERO(EJET_STAT,10)
      ENDIF
C
C ****  INITIALISATION
C
      TOP_TOP_EJET = .FALSE.
      PASS_L2 = .FALSE.
      FLAG_ELEC = .FALSE.
      FLAG_JETS = .FALSE.
      FLAG_MET = .FALSE.
      EJET_STAT(1) = EJET_STAT(1) + 1
      IF (SELECT_EM_FILTERS) THEN
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C   check whether the event passes an EM filter
        DO II = 1,NFSTRING_REQ
          SEARCH_STRING = FSTRING_REQ(II)
          DO JJ = 1,NFILTON
            PASS_L2=PASS_L2.OR.MATCH_WILD(FNAME_ON(JJ),SEARCH_STRING)
          ENDDO
        ENDDO
      ENDIF
C
C ****  GET THE VERTEX
C
      LVERT=GZVERT(1)
      IF(LVERT.GT.0) ZVERT=Q(LVERT+5)
      IF(ZVERT.EQ.0) THEN
          CALL ERRMSG('TOP_TOP_EJET','ZVERT',
     &      'ZVERT is set to ZERO','W')
      ENDIF
C
C ****  Find the Electron
C
      LPELC = GZPELC()
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPELC must be refetched
      LPELC = GZPELC()
      CALL ZSORT(IXCOM,LPELC,7)
      CALL ZTOPSY(IXCOM,LPELC)
      LPELC = GZPELC()
      NELEC = 0
      DO WHILE (LPELC .GT. 0)
        ETA = Q(LPELC+19)   ! Calorimeter eta of cluster (IETA)
        ET = Q(LPELC+7)    ! Et
        IF (Q(LPELC+17) .GT. 0) THEN
          ISOL =(Q(LPELC+16)-Q(LPELC+17))/Q(LPELC+17) ! isolation frac
        ELSE
          ISOL = -100.  ! passing value
        ENDIF
        IF(ABS(ETA) .LT. ELEC_ETACUT
     &     .AND. ET .GT. ELEC_ETCUT
     &     .AND. ISOL .LT. ELEC_ISOLCUT)THEN
          FLAG_ELEC = .TRUE.
          NELEC = NELEC + 1
          LEPPHO_PNTR = LPELC
        ENDIF
        LPELC = LQ(LPELC)
      ENDDO
C
C ****  Select on Photons if requested and required
C
      IF(FLAG_ELEC)GOTO 200
      IF(SELECT_GAMMA)THEN
        LPPHO = GZPPHO()
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LPELC must be refetched
        LPPHO = GZPPHO()
        CALL ZSORT(IXCOM,LPPHO,7)
        CALL ZTOPSY(IXCOM,LPPHO)
        LPPHO = GZPPHO()
        NPHOT = 0
        DO WHILE (LPPHO .GT. 0)
          ETA = Q(LPPHO+19)   ! Calorimeter eta of cluster (IETA)
          ET = Q(LPPHO+7)    ! Et
          IF (Q(LPPHO+17) .GT. 0) THEN
            ISOL =(Q(LPPHO+16)-Q(LPPHO+17))/Q(LPPHO+17) ! isolation frac
          ELSE
            ISOL = -100.  ! passing value
          ENDIF
          IF(ABS(ETA) .LT. ELEC_ETACUT
     &     .AND. ET .GT. ELEC_ETCUT
     &     .AND. ISOL .LT. ELEC_ISOLCUT)THEN
            FLAG_ELEC = .TRUE.
            NPHOT = NPHOT + 1
            LEPPHO_PNTR = LPPHO
          ENDIF
          LPPHO = LQ(LPPHO)
        ENDDO
      ENDIF
  200 CONTINUE
C
C ****  Now look at Jets
C
      IF ((JET_ALGO .GE. 0) .AND. (JET_ALGO .LE. 3))  THEN
        CALL SET_CAPH('CONE_JET',TEMPLATE(1,JET_ALGO),IER)
      ELSEIF (JET_ALGO.EQ.4) THEN
        CALL SET_CAPH('NN_JET',TEMPLATE(1,4),IER)
      ELSE
        CALL ERRMSG('BAD JET_ALGO','TOP_TOP_EJET','Error
     &    setting CAPH path, check RCP','F')
      ENDIF
      LCAPH = GZCAPH()
      LJETS = GZJETS()
C          sort banks so they are in increasing order of Et
C          NOTE: after each reordering of banks the pointer
C                LJETS must be refetched
C        loop over all jets but skip one with electron
C
      CALL ZSORT(IXCOM,LJETS,6)
      LJETS = GZJETS()
      CALL ZTOPSY(IXCOM,LJETS)
      LJETS = GZJETS()
      NJET = 0
      DO WHILE (LJETS .GT. 0)
        LJNEP=LQ(LJETS-2)
        IF ( LQ(LJETS-3).NE.LEPPHO_PNTR) THEN
          THETA = Q(LJETS+7)
          CALL DET_ETA(ZVERT,THETA,ETA)   ! detector eta
          ET = Q(LJETS+6) ! Et
          IF(ABS(ETA) .LT. JET_ETACUT
     &      .AND. ET .GT. JET_ETCUT) THEN
            NJET = NJET + 1
          ENDIF
          IF(NJET.GE.NJET_MIN)THEN
            FLAG_JETS = .TRUE.
          ENDIF
        ENDIF
          LJETS = LQ(LJETS)
      ENDDO
C
C ****  MISSING ET
C
      LPNUT3 = GZPNUT(3)    ! pick missing ET bank with ICD & MUON correction
      LPNUT = GZPNUT(2)    ! pick missing ET bank with ICD correction
      IF (LPNUT .EQ. 0)LPNUT = GZPNUT(1)
      IF (Q(LPNUT+7).GE.MET_CUT .OR. Q(LPNUT3+7).GE.MET_CUT) THEN
        FLAG_MET = .TRUE.
      ENDIF
C
C ****  Collect statistics for cuts
C
      EJET_STAT(1) = EJET_STAT(1) + 1
      IF(PASS_L2) EJET_STAT(2) = EJET_STAT(2) + 1
      IF(PASS_L2.AND.FLAG_ELEC) EJET_STAT(3) = EJET_STAT(3) + 1
      IF(PASS_L2.AND.FLAG_JETS) EJET_STAT(4)=EJET_STAT(4)+1
      IF(PASS_L2.AND.FLAG_MET) EJET_STAT(5)=EJET_STAT(5)+1
      IF(PASS_L2.AND.FLAG_ELEC.AND.FLAG_JETS)
     &    EJET_STAT(6)=EJET_STAT(6)+1
      IF(PASS_L2.AND.FLAG_ELEC.AND.FLAG_MET)
     &    EJET_STAT(7)=EJET_STAT(7)+1
      IF(FLAG_ELEC.AND.FLAG_JETS.AND.FLAG_MET)
     &    EJET_STAT(8)=EJET_STAT(8)+1
C
C ****  FLAG EVENT TO WRITE OUT
C
      IF(EXPRESS_STREAMING)THEN
        IF(PASS_L2.AND.FLAG_ELEC.AND.FLAG_MET)THEN
          EJET_STAT(9)=EJET_STAT(9)+1
          TOP_TOP_EJET = .TRUE.
        ENDIF
      ELSE
        IF(PASS_L2.AND.FLAG_ELEC.AND.FLAG_JETS.AND.FLAG_MET)THEN
          EJET_STAT(9)=EJET_STAT(9)+1
          TOP_TOP_EJET = .TRUE.
        ENDIF
      ENDIF
      CALL RESET_CAPH
  999 RETURN
C****************************************************************************
      ENTRY TOP_TOP_EJET_EOJ(TOP_TOP_EJET_SUM)
C----------------------------------------------------------------------------
C-   Purpose and Methods : End of job summary for TOP_TOP_EJET filter.
C-                          (Modified from Dhiman's code)
C-   Inputs  : none
C-   Outputs : TOP_TOP_EJET_SUM(20) real array of summary results:
C-             (1) Number of events processed
C-             (2) Number of events satisfying the filter requirement
C-             (3) Number of events surviving filter and ELEC cuts 
C-             (4) Number of events surviving filter and JET cuts
C-             (5) Number of events surviving filter and Missing_Et cut
C-             (6) Number of events surviving filter, ELEC and JET cuts
C-             (7) Number of events surviving filter,ELEC and Missing_Et cuts
C-             (8) Number of events surviving ELEC, JETS and Missing_Et cuts
C-             (9) Number of events surviving all cuts ( events written out)
C-
C-             (11-20)  RCP info
C----------------------------------------------------------------------------
      DO III = 1,9
        TOP_TOP_EJET_SUM(III) = EJET_STAT(I)
      ENDDO
C
      TOP_TOP_EJET_SUM(11) = JET_ALGO
      TOP_TOP_EJET_SUM(12) = ELEC_ETCUT
      TOP_TOP_EJET_SUM(13) = ELEC_ETACUT
      TOP_TOP_EJET_SUM(14) = ELEC_ISOLCUT
      TOP_TOP_EJET_SUM(15) = NJET_MIN 
      TOP_TOP_EJET_SUM(16) = JET_ETCUT 
      TOP_TOP_EJET_SUM(17) = JET_ETACUT
      TOP_TOP_EJET_SUM(18) = MET_CUT
C      TOP_TOP_EJET_SUM(19) = EXPRESS_STREAMING
C      TOP_TOP_EJET_SUM(20) = SELECT_GAMMA
      TOP_TOP_EJET_EOJ = .TRUE.
      END
