      LOGICAL FUNCTION QCD_GAMMA()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :  Filter direct photon events for DSTs on disk.
C-                          Drop events where the photon Et is less than
C-                          the level 2 threshold.  Also drop unisolated
C-                          single photon events.
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  13-DEC-1992   sFahey
C-   Updated  17-FEB-1992   sFahey  Added RCP file and QCD_GAMMA_EOJ
C-   Updated   7-JUL-1993   sFahey  Added PELC bank info
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$INC:ZLINKC.INC'
C
      LOGICAL FIRST,FIRED(20),PASSED(20),QCD_GAMMA_EOJ
      LOGICAL USE_PPHO,USE_PELC
      REAL ET,FILT_THRSHOLDS(20),FILT_MET(20)
      REAL ECORE,EISO,ETCORE,ETISO,THETA,ISO
      REAL FILT_ISO(20),GAM_STAT(20),GAM_SUMRY(20)
      INTEGER RUNNUM,NUM_FILT_NAMES,JUNKNUM,I,IER,LENGTH
      INTEGER GZPPHO,GZPELC,GZPNUT,MET,METFRAC
      CHARACTER*32 FILTNAMES(20),TRIGNON(32),FILTNON(128)
      INTEGER NTRIGON,TRIGBON(32),NFILTON,FILTBON(128)
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
C
      IF (FIRST) THEN
        FIRST = .FALSE.
        CALL INRCP('QCD_GAMMA_RCP',IER)
        IF (IER.EQ.0) THEN
          CALL EZPICK('QCD_GAMMA_RCP')
          CALL EZGET('USE_PPHO',USE_PPHO,IER)
          CALL EZGET('USE_PELC',USE_PELC,IER)
          CALL EZGET('NUM_FILT_NAMES',NUM_FILT_NAMES,IER)
          DO I=1,NUM_FILT_NAMES
            CALL EZGETS('FILT_NAMES',I,FILTNAMES(I),LENGTH,IER)
          ENDDO
          CALL EZGET('FILT_THRSHOLDS',FILT_THRSHOLDS,IER)
          CALL EZGET('FILT_ISO',FILT_ISO,IER)
          CALL EZGET('FILT_MET',FILT_MET,IER)
          CALL EZRSET
          CALL VZERO(GAM_STAT,20)
        ELSE
          CALL ERRMSG('QCD_GAMMA','QCD_GAMMA_RCP',
     &         'ERROR READING QCD_GAMMA RCP FILE','F')
        ENDIF
      ENDIF
C

      QCD_GAMMA = .FALSE.
      DO I=1,20
        PASSED(I) = .FALSE.
      ENDDO
C
      CALL GTTSUM(NTRIGON,TRIGBON,TRIGNON,NFILTON,FILTBON,FILTNON)
      DO I=1,NUM_FILT_NAMES
        CALL GET_L2_BIT_NUMBER(FILTNAMES(I),JUNKNUM,FIRED(I))
      ENDDO
C
      LPNUT = GZPNUT(2)
      MET = Q(LPNUT+7)
C
      IF (USE_PPHO) THEN
        LPPHO = GZPPHO()
        DO WHILE (LPPHO.GT.0)
          ET = Q(LPPHO+7)
          METFRAC = MET/ET
          THETA = Q(LPPHO+8)
          ECORE = Q(LPPHO+15)
          EISO  = Q(LPPHO+16)
          ETCORE = ECORE*SIN(THETA)     ! CONVERT E TO ET IN CORE(R=.2) CONE
          ETISO = EISO*SIN(THETA)       ! CONVERT E TO ET IN CORE(R=.4) CONE
          ISO = ETISO - ETCORE
          DO I=1,NUM_FILT_NAMES
            IF ((FIRED(I)).AND.(ET.GT.FILT_THRSHOLDS(I)).AND.
     &         (ISO.LT.FILT_ISO(I)).AND.(METFRAC.LT.FILT_MET(I))) THEN
              QCD_GAMMA = .TRUE.
              PASSED(I) = .TRUE.
            ENDIF
          ENDDO
          LPPHO = LQ(LPPHO)
        ENDDO
      ENDIF
C
      IF (USE_PELC) THEN
        LPELC = GZPELC()
        DO WHILE (LPELC.GT.0)
          ET = Q(LPELC+7)
          METFRAC = MET/ET
          THETA = Q(LPELC+8)
          ECORE = Q(LPELC+15)
          EISO  = Q(LPELC+16)
          ETCORE = ECORE*SIN(THETA)     ! CONVERT E TO ET IN CORE(R=.2) CONE
          ETISO = EISO*SIN(THETA)       ! CONVERT E TO ET IN CORE(R=.4) CONE
          ISO = ETISO - ETCORE
          DO I=1,NUM_FILT_NAMES
            IF ((FIRED(I)).AND.(ET.GT.FILT_THRSHOLDS(I)).AND.
     &         (ISO.LT.FILT_ISO(I)).AND.(METFRAC.LT.FILT_MET(I))) THEN
              QCD_GAMMA = .TRUE.
              PASSED(I) = .TRUE.
            ENDIF
          ENDDO
          LPELC = LQ(LPELC)
        ENDDO
      ENDIF
C
C   SUMMARIES FOR THE 1ST 10 LEVEL2 FILTER NAMES
C
      DO I=1,10
        IF (FIRED(I)) GAM_STAT(I) = GAM_STAT(I) + 1.0
        IF (PASSED(I)) GAM_STAT(I+10) = GAM_STAT(I+10) + 1.0
      ENDDO
C
  999 RETURN
C---------------------------------------------------------------------
      ENTRY QCD_GAMMA_EOJ(GAM_SUMRY)
C---------------------------------------------------------------------
C-
C-  Purpose and Methods : EOJ stats from QCD_GAMMA
C-
C-      GAM_SUMRY(1-10) # of events for 1st 10 L2 filts in RCP file
C-
C-      GAM_SUMRY(11-20) # of events passed by QCD_GAMMA for filts
C-
C---------------------------------------------------------------------
      QCD_GAMMA_EOJ = .TRUE.
      CALL UCOPY(GAM_STAT,GAM_SUMRY,20)
      RETURN
      END
