      SUBROUTINE CSF_HISTS(ICH,NCH,IETA,IPHI,ILYR,ENERGY)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Histograms of energy flow as a function of phi
C-                         for all modules
C-
C-   Inputs  : ICH = channel number
C-             NCH = TOTAL NUMBER OF CHANNELS
C-             IETA = eta index
C-             IPHI=PHI INDEX
C-             ILYR = LAYER NUMBER
C-             ENERGY = ENERGY IN CELL
C-   Outputs :
C-   Controls:
C-
C-   Created  26-NOV-1995   Rajendran Raja
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
      INTEGER LVERT,GZVERT
      INTEGER ICH,NCH,IETA,IPHI,ILYR
      INTEGER IER
      REAL    ENERGY
      INTEGER MODMX
      PARAMETER( MODMX = 17 )
      REAL    EFLOW(64,MODMX)
      LOGICAL DIRTY,SUPER_CLEAN,MRVETO
      SAVE DIRTY,SUPER_CLEAN
      INTEGER I,J
      INTEGER IOFF,IDEPTH
C
      INTEGER L2EM,GZL2EM
      INTEGER IETA_TR,IPHI_TR,ILYR_TR
      INTEGER TRIGGER_PHI(64,MODMX)
C
      INTEGER ID
C
      INTEGER IMODULE,CAL_MODULE
      CHARACTER*32 NAME
C
      INTEGER MX_FILT
      PARAMETER( MX_FILT = 20 )
      CHARACTER*32 FILT_NAMES(MX_FILT)
      SAVE FILT_NAMES
C
      INTEGER NFILTS
      SAVE NFILTS
      LOGICAL FILT_PASSED
      SAVE FILT_PASSED
C
      INTEGER NTRIGON,NFILTON,TBIT_ON(32),FBIT_ON(128)
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 SEARCH_STRING
C
      LOGICAL MATCH_WILD
      INTEGER RUNNO
      REAL    RUN
C
      INTEGER NO_BAD_RUN_MAX
      PARAMETER( NO_BAD_RUN_MAX = 10000 )
      INTEGER BAD_RUN_NOS(NO_BAD_RUN_MAX),NO_BAD_RUNS
      SAVE BAD_RUN_NOS,NO_BAD_RUNS
      LOGICAL BAD_RUN
      INTEGER IRUN
      SAVE BAD_RUN
C
      LOGICAL first
      SAVE first
      DATA first / .true. /
C----------------------------------------------------------------------
      IF( first ) THEN
        first = .false.
        CALL EZPICK('CAHITS_RCP')
        CALL DHDIR('CAHITS_RCP','HBOOK_PHI_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('CALORIMETER','CSF_HISTS',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL EZ_GET_CHARS('HIST_FILTER_NAMES',NFILTS,FILT_NAMES,IER)
        CALL EZRSET
C
        CALL CSF_BOOK_HISTS
C
        CALL INRCP('D0$CALOR_OFF:CSF_BAD_RUN.RCP',IER)
C
        CALL EZPICK('CSF_BAD_RUN_RCP',IER)
        CALL EZGETA('BAD_RUN_NOS',0,0,0,NO_BAD_RUNS,IER)
        IF(NO_BAD_RUNS.LE.NO_BAD_RUN_MAX) THEN
          IF(IER.EQ.0) CALL EZGET('BAD_RUN_NOS',BAD_RUN_NOS,IER)
        ELSE
          CALL ERRMSG('CALORIMETER','CSF_HISTS',
     &      'TOO MANY BAD RUNS TO STORE ','W')
        ENDIF
C
      ENDIF
C
      IF ( ICH.EQ.1 ) THEN
C 1ST CELL CREATE/SET DIRECTORY
        DIRTY = MRVETO('GOOD_CAL')
        SUPER_CLEAN = .NOT.DIRTY
        IF ( DIRTY ) THEN
          RETURN
        ENDIF
C
        FILT_PASSED = .FALSE.
C
C   get names of triggers/filters fired for this event
C
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C CHECK WHETHER PASSES FILTER
        DO I = 1,NFILTS
          SEARCH_STRING = FILT_NAMES(I)
          DO J = 1,NFILTON
            FILT_PASSED = MATCH_WILD(FNAME_ON(J),SEARCH_STRING)
            IF ( FILT_PASSED ) THEN
              GO TO 123
            ENDIF
          ENDDO
        ENDDO
C
        IF(.NOT.FILT_PASSED)RETURN
  123   CONTINUE
        BAD_RUN = .FALSE.
        IRUN = RUNNO()
        DO I = 1, NO_BAD_RUNS
          IF ( IRUN.LT.BAD_RUN_NOS(I) ) THEN
            GO TO 124
          ELSEIF ( IRUN.EQ.BAD_RUN_NOS(I) ) THEN
            BAD_RUN = .TRUE.
            CALL ERRMSG('CALORIMETER','CSF_HISTS',
     &        'BAD RUN BEING REJECTED ','W')
            GO TO 124
          ENDIF
        ENDDO
  124   CONTINUE
        IF(BAD_RUN)RETURN
C
        CALL EZPICK('CAHITS_RCP')
        CALL DHDIR('CAHITS_RCP','HBOOK_PHI_DIRECTORY',IER,' ')
C         ! Create/Set HBOOK directory
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG('CALORIMETER','CSF_HISTS',
     &        ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL EZRSET
C
C ****  vertex hists
C
        LVERT = GZVERT(1)
        CALL HFILL(41,Q(LVERT+3),0.0,1.0)  !X
        CALL HFILL(42,Q(LVERT+4),0.0,1.0)  !Y
        CALL HFILL(43,Q(LVERT+5),0.0,1.0)  !Z
C
        RUN = RUNNO()
        CALL HFILL(71,RUN,0.0,1.0)
C
        DO I = 1 , MODMX
          DO J = 1 , 64
            EFLOW(J,I) = 0.0
* ENERGY FLOW IN MODULE,PHI PER EVENT
          ENDDO
        ENDDO
*
C
        DO I = 1, MODMX
          DO J = 1, 64
            TRIGGER_PHI(J,I) = 0
          ENDDO
        ENDDO
        L2EM = GZL2EM()
        DO WHILE (L2EM.GT.0)

          IETA_TR = IQ(L2EM+6)
          IPHI_TR = IQ(L2EM+7)
          ILYR_TR = IQ(L2EM+8)
          IF ( IABS(IETA_TR).LE.12.AND.ILYR_TR.LE.7 ) THEN
C CCEM
            TRIGGER_PHI(IPHI_TR,1) = 1
          ENDIF
          IF ( IETA_TR.LE.-14.AND.ILYR_TR.LE.7 ) THEN
C NECEM
            TRIGGER_PHI(IPHI_TR,6) = 1

          ENDIF
          IF ( IETA_TR.GE.14.AND.ILYR_TR.LE.7 ) THEN
C PECEM
            TRIGGER_PHI(IPHI_TR,13) = 1

          ENDIF
          L2EM = LQ(L2EM)
        ENDDO
C
      ENDIF
C
      IF ( SUPER_CLEAN.AND.FILT_PASSED.AND.(.NOT.BAD_RUN) ) THEN
        IMODULE=CAL_MODULE(IETA,ILYR,NAME)
*
        IF (IMODULE.EQ.1) IOFF = 1  !CCEM
        IF (IMODULE.EQ.6) IOFF = 2  !CCFH
        IF (IMODULE.EQ.9) IOFF = 3  !CCCH
*
*
        IF (IMODULE.EQ.3.AND.IETA.LT.0) IOFF = 4  !NCCMG
        IF (IMODULE.EQ.4.AND.IETA.LT.0) IOFF = 5  !NICD
        IF (IMODULE.EQ.2.AND.IETA.LT.0) IOFF = 6  !NECEM
        IF (IMODULE.EQ.7.AND.IETA.LT.0) IOFF = 7  !NECIH
        IF (IMODULE.EQ.8.AND.IETA.LT.0) IOFF = 8  !NECMH
        IF (IMODULE.EQ.10.AND.IETA.LT.0) IOFF = 9 !NECOH
        IF (IMODULE.EQ.5.AND.IETA.LT.0) IOFF = 10 !NECMG
*
        IF (IMODULE.EQ.3.AND.IETA.GT.0) IOFF = 11  !PCCMG
        IF (IMODULE.EQ.4.AND.IETA.GT.0) IOFF = 12  !PICD
        IF (IMODULE.EQ.2.AND.IETA.GT.0) IOFF = 13  !PECEM
        IF (IMODULE.EQ.7.AND.IETA.GT.0) IOFF = 14  !PECIH
        IF (IMODULE.EQ.8.AND.IETA.GT.0) IOFF = 15  !PECMH
        IF (IMODULE.EQ.10.AND.IETA.GT.0) IOFF = 16 !PECOH
        IF (IMODULE.EQ.5.AND.IETA.GT.0) IOFF = 17  !PECMG
*
        IF ( IABS(IETA).LE.32) THEN
*use only when there are at least 64 phis guaranteed
          EFLOW(IPHI,IOFF) = EFLOW(IPHI,IOFF) + ENERGY
          CALL HFILL(IOFF,FLOAT(IETA),FLOAT(IPHI),ENERGY)  !LEGO

          IF ( IOFF.EQ.1.OR.IOFF.EQ.6.OR.IOFF.EQ.13 ) THEN
C EM MODULES DEPTH PLOT
            IDEPTH= ILYR
            IF ( ILYR.GE.3.AND.ILYR.LE.6 ) THEN
              IDEPTH = 3
            ENDIF
            IF(ILYR.EQ.7)IDEPTH=4
            ID = 100*IOFF+10000 + 1000
            CALL HFILL(ID+IDEPTH,FLOAT(IETA),FLOAT(IPHI),ENERGY)
          ENDIF
        ENDIF
*
        CALL HFILL(45,FLOAT(IETA),0.0,ENERGY)
C
        IF ( ICH.EQ.NCH ) THEN
C EVENT COMPLETE HERE
          DO  I = 1 , MODMX
            IOFF = 100*I
            DO J = 1 ,64
              CALL HFILL(IOFF+J,EFLOW(J,I),0.0,1.0)
            ENDDO
          ENDDO
          DO  I = 1 , MODMX
            IF ( I.EQ.1.OR.I.EQ.6.OR.I.EQ.13 ) THEN
              IOFF = 100*I+10000
              DO J = 1 ,64
                IF ( TRIGGER_PHI(J,I).EQ.0 ) THEN
                  CALL HFILL(IOFF+J,EFLOW(J,I),0.0,1.0)
C PLOT ONLY NON TRIGGERED PHIS
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
  999 RETURN
      END
