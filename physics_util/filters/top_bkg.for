      LOGICAL FUNCTION TOP_BKG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Filter fake e background events using based on
C-                         the following criteria:
C-
C-                         1.  Level 2 filters.
C-
C-                         2.  Number of jets.
C-
C-                         3.  Number of highly electromagnetic jets.
C-
C-   Controls: TOP_BKG_RCP
C-
C-   Returned value  : .TRUE. = pass
C-
C-   Alternate ENTRY point: TOP_BKG_EOJ - Print statistics.
C-
C-   Created  17-Feb-1994   Herbert Greenlee
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      LOGICAL TOP_BKG_EOJ
C-
C- Trigger selection variables.
C-
      LOGICAL SELECT_ON_L2, COUNT_JETS, PASS, OK
      INTEGER NFNAME_REQ, NTRIGON, NFILTON, TBIT_ON(32), FBIT_ON(128)
      CHARACTER*32 TNAME_ON(32),FNAME_ON(128)
      CHARACTER*32 FNAME_REQ(128)
C-
C- Jet counting variables.
C-
      INTEGER JET_ALG
      INTEGER LJETS, GZJETS
      REAL ET, ETA, EMFRAC
      INTEGER NJET, NEMJET
      INTEGER NJET_MIN_BY_FILT(128), NEMJET_MIN_BY_FILT(128)
      REAL JET_MIN_ET, JET_MAX_ETA
      REAL EMJET_MIN_ET, EMJET_MIN_EMFRAC, EMJET_MAX_ETA
C-
C- Statistics.
C-
      INTEGER NEVENT, NPASS, NUM_BY_FILT(128), NPASS_BY_FILT(128)
      REAL PASS_RATE, PASS_RATE_BY_FILT(128)
C-
C- Miscellanious.
C-
      INTEGER I, J, IER
      LOGICAL FIRST
C-
      DATA FIRST/.TRUE./
      DATA NEVENT, NPASS, NUM_BY_FILT, NPASS_BY_FILT/258*0/
C----------------------------------------------------------------------
      IF(FIRST) THEN
        FIRST=.FALSE.
        CALL INRCP('TOP_BKG_RCP',IER)
        IF(IER.NE.0)THEN
          CALL ERRMSG('TOP_BKG','TOP_BKG_RCP',
     &      'Error reading RCP file','F')
        ENDIF
        CALL EZPICK('TOP_BKG_RCP')
        CALL EZGET_l('SELECT_ON_L2', SELECT_ON_L2, IER)
        IF(IER.EQ.0)CALL EZGET_l('COUNT_JETS', COUNT_JETS, IER)
        IF(IER.EQ.0)CALL EZ_GET_CHARS('FILTNAMES',NFNAME_REQ,FNAME_REQ, 
     &    IER)
        IF(IER.EQ.0)CALL EZGET_i('JET_ALG', JET_ALG, IER)
        IF(IER.EQ.0)CALL EZGET('JET_MIN_ET', JET_MIN_ET, IER)
        IF(IER.EQ.0)CALL EZGET('JET_MAX_ETA', JET_MAX_ETA, IER)
        IF(IER.EQ.0)CALL EZGET('EMJET_MIN_ET', EMJET_MIN_ET, IER)
        IF(IER.EQ.0)CALL EZGET('EMJET_MIN_EMFRAC', EMJET_MIN_EMFRAC, 
     &    IER)
        IF(IER.EQ.0)CALL EZGET('EMJET_MAX_ETA', EMJET_MAX_ETA, IER)
        IF(IER.EQ.0)CALL EZGET_arr('NJET_MIN_BY_FILT', NJET_MIN_BY_FILT
     &       ,IER)
        IF(IER.EQ.0)CALL EZGET_iarr('NEMJET_MIN_BY_FILT',
     &       NEMJET_MIN_BY_FILT,IER)
        IF (IER.NE.0) THEN
          CALL ERRMSG('TOP_BKG','TOP_BKG_RCP',
     &        'Error getting TOP_BKG RCP values','F')
        ENDIF
        CALL EZRSET
      ENDIF
C
C ****  intializations
C
      TOP_BKG = .FALSE.
      PASS   = .FALSE.
      NJET = 0
      NEMJET = 0
      NEVENT = NEVENT + 1
C-
C- Apply selection cuts.
C-
      IF (.NOT.SELECT_ON_L2) THEN
        PASS = .TRUE.
      ELSE
C-
C- Count D0RECO jet objects.
C-
        IF(COUNT_JETS)THEN
          CALL SET_CAPH_ALG(JET_ALG)
          LJETS = GZJETS()
          DO WHILE(LJETS.GT.0)
            ET = Q(LJETS+6)
            ETA = Q(LJETS+9)
            EMFRAC = Q(LJETS+14)
            IF(ET.GE.JET_MIN_ET .AND. ABS(ETA).LE.JET_MAX_ETA)
     &        NJET = NJET + 1
            IF(ET.GE.EMJET_MIN_ET .AND. ABS(ETA).LE.EMJET_MAX_ETA
     &        .AND. EMFRAC.GE.EMJET_MIN_EMFRAC)NEMJET = NEMJET + 1
            LJETS = LQ(LJETS)
          ENDDO
          CALL RESET_CAPH
        ENDIF
C
C   get names of triggers/filters fired for this event
C
        CALL GTTSUM(NTRIGON,TBIT_ON,TNAME_ON,NFILTON,FBIT_ON,FNAME_ON)
C-
C- Check for an exact match and apply filter-dependent D0RECO jet 
C- requirements.
C-
        DO I = 1,NFNAME_REQ
          DO J = 1,NFILTON
            OK = FNAME_ON(J) .EQ. FNAME_REQ(I)
            IF(OK)NUM_BY_FILT(I) = NUM_BY_FILT(I) + 1
            IF(OK .AND. COUNT_JETS)OK = NJET.GE.NJET_MIN_BY_FILT(I)
     &        .AND. NEMJET.GE.NEMJET_MIN_BY_FILT(I)
            IF(OK)THEN
              PASS = .TRUE.
              NPASS_BY_FILT(I) = NPASS_BY_FILT(I) + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C-
C- Did event pass?
C-
      IF(PASS)THEN
        TOP_BKG = .TRUE.
        NPASS = NPASS + 1
      ENDIF
      GO TO 999

      ENTRY TOP_BKG_EOJ()
C-
C- Entry point to print statistics.
C-
      TOP_BKG_EOJ = .TRUE.
C-
C- Calculate pass rates.
C-
      IF(NEVENT.GT.0)THEN
        DO I = 1,NFNAME_REQ
          PASS_RATE_BY_FILT(I) = FLOAT(NPASS_BY_FILT(I))/FLOAT(NEVENT)
        ENDDO
        PASS_RATE = FLOAT(NPASS)/FLOAT(NEVENT)
      ELSE
        DO I = 1,NFNAME_REQ
          PASS_RATE_BY_FILT(I) = 0.
        ENDDO
        PASS_RATE = 0.
      ENDIF
      PRINT 10, NEVENT, NPASS, PASS_RATE, 
     &  (FNAME_REQ(I),NUM_BY_FILT(I), NPASS_BY_FILT(I),
     &   PASS_RATE_BY_FILT(I), I=1,NFNAME_REQ)
 10   FORMAT(
     &  /' TOP_BKG filter statistics'
     &  //1X,I8,' Events processed'
     &  //' Filter                          Number    Passed',
     &    '    Pass rate'
     &   /' ------                          ------    ------',
     &    '    ---------'
     &  //' Total',T34,10X,I6,4X,G9.3
     &  //(1X,A32,I6,4X,I6,4X,G9.3))
  999 RETURN
      END
