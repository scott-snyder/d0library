      FUNCTION CALOR_HIST ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  22-MAR-1991   Chip Stewart, Boaz Klima
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL CALOR_HIST
      LOGICAL CALOR_HIST_INI
      LOGICAL CALOR_HIST_END
      LOGICAL CALOR_HIST_RESET
      LOGICAL OK,FIRST
      LOGICAL CAD,CAEP,PEDS,GAINS,START,GEANT
      INTEGER I,J,K,L,ID
      INTEGER IER,NCH,ICAD,CRATE,IWORD,PH,NCH1,NCH2
      INTEGER NV,NR,IETA,IPHI,ILYR,BITS,ICHAN
      INTEGER ICRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM,SEQ
      REAL    ENERGY,ENERGY_TOT,PEDESTAL,GAIN,SIGMA
      INCLUDE 'D0$PARAMS:CAL_ADC_NO.PARAMS'     ! CAD bank params
      SAVE FIRST,OK
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      CALOR_HIST=.TRUE.
      CALL DHDIR('CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
      IF ( IER.NE.0 ) THEN
        CALL ERRMSG(' NO_HBOOK_DIR','CALOR_HIST',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
      ENDIF
C
      IF (CAEP) THEN
        CALL GTCAEP_HEADER(NV,NR,NCH,IER)
        CALL HF1(201,FLOAT(NCH),1.0)
        START = .TRUE.
        ENERGY_TOT = 0
        DO 17,  I = 1, NCH
          CALL GTCAEP(START,IETA,IPHI,ILYR,BITS,ENERGY,ICHAN,IER)
          START = .FALSE.
          CALL HF1(200,ENERGY,1.0)
          CALL HF2(202,FLOAT(IETA),FLOAT(IPHI),ENERGY)
          ENERGY_TOT = ENERGY_TOT + ENERGY
  17    CONTINUE
        CALL HF1(203,ENERGY_TOT,1.0)
      END IF
      IF (PEDS .OR. GAINS .OR. CAD) THEN
        CALL GTCAD_TOTAL(1,NCH1,IER)
        CALL GTCAD_TOTAL(2,NCH2,IER)
        IF (CAD) CALL HF1(101,FLOAT(NCH1+NCH2),1.0)
        DO ICAD = 1, 2
          CALL GTCAD_TOTAL(ICAD,NCH,IER)
          START = .TRUE.
          DO 27,  I = 1, NCH
            CALL GTCAD(ICAD,START,CRATE,IWORD,IER)
            START    = .FALSE.
            IF (CAD) THEN
              CALL CALPH(IWORD,PH)
              CALL HF1(100,FLOAT(PH)+0.5,1.0)
            END IF
            IF (PEDS .OR. GAINS) THEN
              CALL CADUPK(CRATE,IWORD,
     &        ICRATE,ADC,BLS,ROTOW,DEPTH,SCALE,NEGLIM)
              CALL CADPH(CRATE,ADC,BLS,ROTOW,DEPTH,IETA,IPHI,ILYR,IER)
              IF (IER.NE.0) GOTO 27
              SEQ=  BLS*NDEPTC*NEFC + ROTOW*NDEPTC + DEPTH
              IF (ILYR.LE.2) THEN
                ID = 11
              ELSE IF (ILYR.LE.7) THEN
                ID = 12
              ELSE 
                ID = 13
              END IF
              IF (PEDS) THEN
                CALL GT_PED_GNS_ADDR(1,CRATE,ADC,SEQ,SCALE,
     &          PEDESTAL,SIGMA,IER)
                CALL HF1(300,PEDESTAL,1.0)
                CALL HF1(300+ID,PEDESTAL,1.0)
              END IF
              IF (GAINS) THEN
                CALL GT_PED_GNS_ADDR(3,CRATE,ADC,SEQ,SCALE,
     &          GAIN,SIGMA,IER)
                CALL HF1(400,GAIN,1.0)
                CALL HF1(400+ID,GAIN,1.0)
              END IF
            END IF
  27      CONTINUE
        END DO
      END IF
      IF(GEANT) CALL CAD_CHECK_GEANT
  999 RETURN
C
C----------------------------------------------------------------------
C
      ENTRY CALOR_HIST_INI ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : INTIALIZE CALOR_HIST PACKAGE
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-SEP-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        FIRST=.FALSE.
        CALL INRCP('CALOR_HIST_RCP',IER)       ! read in RCP file
        IF(IER.NE.0) GOTO 1999              ! failed
C
        CALL INRCPE('CALOR_HIST_RCPE',IER)     ! read overwrite file (RCPE)
        IF(IER.EQ.0)
     &  CALL ERRMSG(' FOUND_CALOR_HIST_RCPE','CALOR_HIST_INI',
     &  ' Default CALOR_HIST_RCP modified','W')
        CALL EZPICK('CALOR_HIST_RCP')              ! select CAHITS bank
        CALL EZERR(IER)
        IF(IER.EQ.0) THEN
          CALOR_HIST_INI=.TRUE.
          CALL EZGET('CAD_HIST',CAD,IER)
          CALL EZGET('CAEP_HIST',CAEP,IER)
          CALL EZGET('PEDS_HIST',PEDS,IER)
          CALL EZGET('GAINS_HIST',GAINS,IER)
          CALL EZGET('GEANT_CHECK',GEANT,IER)
          CALL DHDIR('CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
          IF ( IER.NE.0 ) THEN
            CALL ERRMSG(' NO_HBOOK_DIR','CALOR_HIST_INI',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
          ENDIF
          IF (CAD) CALL DO_HBOOK('CAD_HISTOGRAMS')
          IF (CAEP) CALL DO_HBOOK('CAEP_HISTOGRAMS')
          IF (PEDS) CALL DO_HBOOK('PEDESTAL_HISTOGRAMS')
          IF (GAINS) CALL DO_HBOOK('GAINS_HISTOGRAMS')
          IF (GEANT) CALL DO_HBOOK('GEANT_HISTOGRAMS')
          CALL EZRSET
        ELSE
          CALL ERRMSG('NO_CALOR_HIST_RCP','CALOR_HIST_INI',
     &      ' CALOR_HIST_RCP bank no found.','W')
        ENDIF
      ENDIF
 1999 RETURN
C
C----------------------------------------------------------------------
      ENTRY CALOR_HIST_END
C----------------------------------------------------------------------
C-
C-   Purpose and Methods :
C-
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  19-SEP-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      CALOR_HIST_END=.TRUE.
 3999 RETURN


      ENTRY CALOR_HIST_RESET ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RESET/CLEAR CALOR_HIST HISTOGRAMS
C-
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: CALOR_HIST_RCP
C-
C-   Created  19-SEP-1990   Chip Stewart
C-
C----------------------------------------------------------------------
      CALOR_HIST_RESET=.FALSE.
      CALL EZPICK('CALOR_HIST_RCP')              ! select CAHITS bank
      CALL EZERR(IER)
      IF(IER.EQ.0) THEN
        CALL DHDIR('CALOR_HIST_RCP','HBOOK_DIRECTORY',IER,' ')
        IF ( IER.NE.0 ) THEN
          CALL ERRMSG(' NO_HBOOK_DIR','CALOR_HIST_RESET',
     &    ' ERROR SETTING HBOOK DIRECTORY ','W')
        ENDIF
        CALL HRESET (0,' ')
        CALL EZRSET
        CALOR_HIST_RESET=.TRUE.
      ELSE
        CALL ERRMSG('NO_CALOR_HIST_RCP','CALOR_HIST_RESET',
     &      ' CALOR_HIST_RCP bank no found.','W')
      ENDIF
 4999 RETURN
      END
