      FUNCTION LV0HIS()
C----------------------------------------------------------------------
C
C-   Purpose and Methods : Booking and filling histograms for
C-                         LEVEL0 package
C-
C-   Returned value  : TRUE if histograms successfully filled.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   2-JUN-1992   Jeffrey Bantly
C-   Updated  15-JUL-1992   Rushdy Ahmad   adding more diagnostic histograms
C-   Updated  14-SEP-1992   Jeffrey Bantly  changes for vertex board data
C
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL LV0HIS
C
      INCLUDE 'D0$INC:ZEBCOM.INC/LIST'
      INCLUDE 'D0$LINKS:IZTRGR.LINK'
C
      INTEGER MAXHIS, MAXHIS2
      INTEGER CHANNEL, CATEGORY
      PARAMETER( MAXHIS = 20 )
      PARAMETER( CHANNEL = 72 )
      PARAMETER( CATEGORY = 10 )
      INTEGER ID,NID(MAXHIS),NID2((CATEGORY+1)*100)
      INTEGER ERR,I,J,K,LOW,HIGH,OFFSET
      INTEGER HITN_SC,HITS_SC
      INTEGER HITN_LC,HITS_LC
      INTEGER FBUNCH,LBUNCH
      INTEGER FCHAN,LCHAN
      INTEGER FGROUP,LGROUP
      INTEGER IBUNCH,ICHAN,IGROUP
      INTEGER CBUNCH
      INTEGER FASTZ_ID,VERTEX_BOARD
      INTEGER RAW_TIME(80)
      INTEGER BUNCH_ID(80)
      INTEGER RAW_CHARGE(80)
      INTEGER CORRECT_TIME(80)
      INTEGER GOODZ_SCALER(6,0:14)
      INTEGER NGOODZ_SCALER(6,0:14)
      INTEGER VERTEX_INFO(6,6,8)
      INTEGER LTRGR,LCRATE,LCRATE0
      INTEGER JBUNCH, NCHANNELS, NWORDS
      INTEGER INTERACTION,MI_FLAG
      INTEGER HWORDS(26),SWORDS(26)
      INTEGER GZFIND_CRATE
      EXTERNAL GZFIND_CRATE
C
      REAL LV0_HIST(4,MAXHIS)
      REAL LV0_HIST2(4,CATEGORY)
      REAL NOVRF(MAXHIS),YESVRF(MAXHIS)
      REAL PEDSIG_TMUL,PEDSIG_QMUL
      REAL PED_ARRAY(5,72)
      REAL TIMEPED(72),CHGPED(72)
      REAL FASTZ,SLOW_Z
      REAL MI_QUALITY,FULL_Z,CHAN_EFF(72)
C
      LOGICAL FIRST,LV0,GOODZ,GOODSZ
      LOGICAL STANDARD_HIST2
      LOGICAL EZERROR
      LOGICAL FLGVAL, VRFFLG
      LOGICAL PRODUC, PRODFL
C
      CHARACTER*27 NAME(MAXHIS)
      CHARACTER*24 NAME2(CATEGORY)
      CHARACTER*27 TITLE
C
      SAVE FIRST,NAME,LV0_HIST,LV0_HIST2
      SAVE NOVRF,YESVRF,VRFFLG
      DATA FIRST/.TRUE./
      DATA NAME/'Raw Time','Raw Charge','Correct Time',
     &   'Good Hits Short Cntrs (N)','Good Hits Short Cntrs (S)',
     &   'Good Hits Long Cntrs (N)','Good Hits Long Cntrs (S)',
     &   'FASTZ Vertex','Slow Z Vertex','Correct Bunch',
     &   'Good Hits by Chan (N) Time','Good Hits by Chan (S) Time',
     &   'Good Hits by Chan (N) Chrg','Good Hits by Chan (S) Chrg',
     &   'Short Counters Hit (N)','Short Counters Hit (S)',
     &   'Long Counters Hit (N)','Long Counters Hit (S)',
     &   'Multiple Interact Flag','Multiple Interact Quality' /
      DATA NAME2/'Raw Time  Channel No.','Raw Charge  Channel No.',
     &  'Pedestal(T)  Chan. No.','Pedestal(Q)  Chan. No.',
     &  'Raw Time(OFF) Chan.No.','Raw Charge(OFF) Chan.No.',
     &  'Pedestal(T,O) Chan. No.','Pedestal(Q,O) Chan. No.',
     &  'Correct Time Chan. No.','Tcor(M)-Tcor(E) Chan No' /
      DATA NOVRF/7*0.,1.,1.,9*0.,1.,0./
      DATA YESVRF/3*1.,4*0.,1.,1.,1.,8*0.,1.,1./
C----------------------------------------------------------------------
C
      LV0HIS=.FALSE.
      CALL DHDIR('LEVEL0_RCP','HBOOK_DIRECTORY',ERR,' ')
      IF(ERR.NE.0) THEN
        CALL ERRMSG('LEVEL0-bad-hbk-dir','LV0HIS',
     &          ' ERROR SETTING HBK DIR','W')
C
      ENDIF
C
      IF (FIRST) THEN    ! Book histograms
        CALL EZPICK('LEVEL0_RCP')
        IF ( EZERROR(ERR) ) THEN
          CALL ERRMSG('LEVEL0-no-rcp','LV0HIS',
     &                        'LEVEL0_RCP not found.','W')
        ELSE
          CALL EZGET('LV0_HIST(1)',LV0_HIST(1,1),ERR)
          CALL EZGET('LV0_HIST2(1)',LV0_HIST2(1,1),ERR)
          CALL EZGET('PEDSIG_TMUL',PEDSIG_TMUL,ERR)
          CALL EZGET('PEDSIG_QMUL',PEDSIG_QMUL,ERR)
          CALL EZGET('PED_ARRAY(1)',PED_ARRAY(1,1),ERR)
          CALL EZRSET
        ENDIF
C
        PRODFL = PRODUC()
        VRFFLG = .TRUE.
        IF ( PRODFL ) VRFFLG = FLGVAL('VERIFY')
        DO 100 ID=1,MAXHIS
          IF ( PRODFL ) THEN
            IF ( VRFFLG ) THEN
              LV0_HIST(1,ID) = YESVRF(ID)
            ELSE
              LV0_HIST(1,ID) = NOVRF(ID)
            ENDIF
          ENDIF
          IF (LV0_HIST(1,ID).NE.1.) GO TO 100
          CALL HBOOK1(ID,NAME(ID),
     &      NINT(LV0_HIST(2,ID)),LV0_HIST(3,ID),LV0_HIST(4,ID),0.)
  100   CONTINUE
        DO 101 ID=1,MAXHIS
          IF (LV0_HIST(1,ID).EQ.2.) THEN
            IF ( PRODFL ) THEN
              LV0_HIST(1,ID) = 0.
            ELSE
              CALL HBOOK2(ID,NAME(ID),NINT(LV0_HIST(2,ID)),
     &          LV0_HIST(3,ID),LV0_HIST(4,ID),NINT(LV0_HIST(2,ID+1)),
     &          LV0_HIST(3,ID+1),LV0_HIST(4,ID+1),0.)
            ENDIF
          ENDIF
  101   CONTINUE
C
        STANDARD_HIST2 = .FALSE.
        IF ( PRODFL ) GOTO 202
        DO 200 ID=1,CATEGORY
          IF (LV0_HIST2(1,ID).NE.1.) GO TO 200
          STANDARD_HIST2 = .TRUE.
          DO I=1,CHANNEL
            J=100*ID+I
            WRITE(TITLE,1001)
     &        NAME2(ID),I
            CALL HBOOK1(J,TITLE,
     &        NINT(LV0_HIST2(2,ID)),LV0_HIST2(3,ID),LV0_HIST2(4,ID),0.)
          ENDDO
  200   CONTINUE
        DO 201 ID=1,CATEGORY
          STANDARD_HIST2 = .TRUE.
          IF (LV0_HIST2(1,ID).EQ.2.) THEN
            DO I=1,CHANNEL
              J=100*ID+I
              WRITE(TITLE,1001)
     &            NAME2(ID),I
              CALL HBOOK2(J,TITLE, NINT(LV0_HIST2(2,ID)),
     &            LV0_HIST2(3,ID),LV0_HIST2(4,ID),NINT(LV0_HIST2(2,
     &            ID+1)),
     &            LV0_HIST2(3,ID+1),LV0_HIST2(4,ID+1),0.)
            ENDDO
          ENDIF
  201   CONTINUE
 1001   FORMAT(A24,I2)
  202   CONTINUE
C
        CALL VZERO(TIMEPED,72)
        CALL VZERO(CHGPED,72)
        DO I=1,72
          ICHAN = INT(PED_ARRAY(1,I))
          TIMEPED(ICHAN) = PED_ARRAY(2,I) -
     &                              PEDSIG_TMUL*PED_ARRAY(3,I)
          CHGPED(ICHAN) = PED_ARRAY(4,I) +
     &                              PEDSIG_QMUL*PED_ARRAY(5,I)
        ENDDO
        FIRST = .FALSE.
      END IF
C
      IF ( PRODFL ) THEN
        IF ( .NOT.VRFFLG ) GOTO 800
      ENDIF
C
      CALL L0_COR_BUNCH(CBUNCH)
      IF (LV0_HIST(1,10).EQ.1.) CALL HFF1(10,NID(10),
     &                FLOAT(CBUNCH),1.)
C
      CALL GTL0AD(CBUNCH,JBUNCH,NCHANNELS,NWORDS,RAW_TIME,BUNCH_ID,
     &  RAW_CHARGE,CORRECT_TIME)
C
      HITN_SC = 0
      HITS_SC = 0
      HITN_LC = 0
      HITS_LC = 0
      DO 301 ICHAN=1,72
        IF (LV0_HIST(1,1).EQ.1.) CALL HFF1(1,NID(1),
     &                   FLOAT(RAW_TIME(ICHAN)),1.)
        IF (LV0_HIST(1,2).EQ.1.) CALL HFF1(2,NID(2),
     &                   FLOAT(RAW_CHARGE(ICHAN)),1.)
        IF (LV0_HIST(1,3).EQ.1.) CALL HFF1(3,NID(3),
     &                   FLOAT(CORRECT_TIME(ICHAN)),1.)
        IF ( PRODFL ) GOTO 301
        IF ( CORRECT_TIME(ICHAN).GT.0.0 ) THEN
C        IF ((FLOAT(RAW_CHARGE(ICHAN)).GT.CHGPED(ICHAN)).AND.
C     &      (FLOAT(RAW_TIME(ICHAN)).LT.TIMEPED(ICHAN))) THEN
          IF (ICHAN.LT.21) THEN
            HITN_SC = HITN_SC+1
            IF (LV0_HIST(1,4).EQ.1.) CALL HFF1(4,NID(4),
     &                FLOAT(ICHAN),1.)
          ENDIF
          IF ((ICHAN.GT.36).AND.(ICHAN.LT.57)) THEN
            HITS_SC = HITS_SC+1
            IF (LV0_HIST(1,5).EQ.1.) CALL HFF1(5,NID(5),
     &                 FLOAT(ICHAN),1.)
          ENDIF
          IF ((ICHAN.GT.20).AND.(ICHAN.LT.37)) THEN
            HITN_LC = HITN_LC+1
            IF (LV0_HIST(1,6).EQ.1.) CALL HFF1(6,NID(6),
     &                 FLOAT(ICHAN),1.)
          ENDIF
          IF (ICHAN.GT.56) THEN
            HITS_LC = HITS_LC+1
            IF (LV0_HIST(1,7).EQ.1.) CALL HFF1(7,NID(7),
     &                 FLOAT(ICHAN),1.)
          ENDIF
        ENDIF
        IF (FLOAT(RAW_TIME(ICHAN)).LT.TIMEPED(ICHAN)) THEN
          IF (ICHAN.LT.37) THEN
            IF (LV0_HIST(1,11).EQ.1.) CALL HFF1(11,NID(11),
     &           FLOAT(ICHAN),1.)
          ELSE
            IF (LV0_HIST(1,12).EQ.1.) CALL HFF1(12,NID(12),
     &           FLOAT(ICHAN),1.)
          ENDIF
        ENDIF
        IF (FLOAT(RAW_CHARGE(ICHAN)).GT.CHGPED(ICHAN)) THEN
          IF (ICHAN.LT.37) THEN
            IF (LV0_HIST(1,13).EQ.1.) CALL HFF1(13,NID(13),
     &           FLOAT(ICHAN),1.)
          ELSE
            IF (LV0_HIST(1,14).EQ.1.) CALL HFF1(14,NID(14),
     &           FLOAT(ICHAN),1.)
          ENDIF
        ENDIF
  301 CONTINUE
C
      IF (LV0_HIST(1,15).EQ.1.) CALL HFF1(15,NID(15),
     &                FLOAT(HITN_SC),1.)
      IF (LV0_HIST(1,16).EQ.1.) CALL HFF1(16,NID(16),
     &                FLOAT(HITS_SC),1.)
      IF (LV0_HIST(1,17).EQ.1.) CALL HFF1(17,NID(17),
     &                FLOAT(HITN_LC),1.)
      IF (LV0_HIST(1,18).EQ.1.) CALL HFF1(18,NID(18),
     &                FLOAT(HITS_LC),1.)
C
      IF ( STANDARD_HIST2 ) THEN
        DO IBUNCH=1,6
          CALL GTL0AD(IBUNCH,JBUNCH,NCHANNELS,NWORDS,RAW_TIME,BUNCH_ID,
     &      RAW_CHARGE,CORRECT_TIME)
          OFFSET=4
          IF (IBUNCH.EQ.CBUNCH) THEN ! to get the correct bunch
            OFFSET=0
          ENDIF
          DO I=1,CHANNEL
            J=100*OFFSET+I
            IF (LV0_HIST2(1,1+OFFSET).EQ.1.)
     &        CALL HFF1(J+100,NID2(J+100),FLOAT(RAW_TIME(I)),1.)
            IF (LV0_HIST2(1,2+OFFSET).EQ.1.)
     &        CALL HFF1(J+200,NID2(J+200),FLOAT(RAW_CHARGE(I)),1.)
            IF (LV0_HIST2(1,3+OFFSET).EQ.1.) THEN
              IF ((FLOAT(RAW_TIME(I)).GT.840).AND.
     &           (FLOAT(RAW_TIME(I)).LT.1040)) THEN
                CALL HFF1(J+300,NID2(J+300),FLOAT(RAW_TIME(I)),1.)
              ENDIF
            ENDIF
            IF (LV0_HIST2(1,4+OFFSET).EQ.1.) THEN
              IF ((FLOAT(RAW_CHARGE(I)).GT.0.0).AND.
     &            (FLOAT(RAW_CHARGE(I)).LE.100.))THEN
                CALL HFF1(J+400,NID2(J+400),FLOAT(RAW_CHARGE(I)),
     &            1.)
              ENDIF
            ENDIF
            IF (LV0_HIST2(1,9).EQ.1.)
     &        CALL HFF1(900+I,NID2(I+900),FLOAT(CORRECT_TIME(I)),1.)
C            IF (LV0_HIST2(1,10).EQ.1.) THEN
C              TDIF=FLOAT(CORRECT_TIME(I)-TCOR_E(I))
C              CALL HFF1(1000+I,NID2(I+1000),TDIF,1.)
C            ENDIF
          ENDDO
        ENDDO
      ENDIF
C
C  Fill FASTZ and Slow Z histograms.
C
  800 CONTINUE
      CALL L0_FASTZ_VERTEX(FASTZ,GOODZ)
      IF ( GOODZ ) THEN
        IF (LV0_HIST(1,8).EQ.1.) CALL HFF1(8,NID(8),FASTZ,1.)
      ENDIF
      CALL L0_SLOW_VERTEX(SLOW_Z,MI_FLAG,MI_QUALITY,INTERACTION,
     &                    GOODSZ,FULL_Z,CHAN_EFF)
      IF ( GOODSZ ) THEN
        IF ( INTERACTION.EQ.1 ) THEN
          IF(LV0_HIST(1,9).EQ.1.) CALL HFF1(9,NID(9),SLOW_Z,1.)
          IF(LV0_HIST(1,19).EQ.1.) CALL HFF1(19,NID(19),
     &                                              FLOAT(MI_FLAG),1.)
          IF(LV0_HIST(1,20).EQ.1.) CALL HFF1(20,NID(20),MI_QUALITY,1.)
        ENDIF
      ENDIF
C
C  Done.
C
  990 CONTINUE
      LV0HIS=.TRUE.
C-------------------------------------------------------------------------
  999 RETURN
      END
