      SUBROUTINE L1_CALTRIG_TOWERS( SUCCESS )
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Computes the Trigger Tower ADC bytes from an input
C-                              bank as chosen by the user in the parameter
C-                              L1SIM_INPUT_SOURCE. Fills the TRG_SIMUL_EVENT
C-                              and TRG_SIMUL_RAW_EVENT commons.
C-   Inputs:   None.
C-
C-   Outputs :  SUCCESS [L] this flag is false if no apropriate input data 
C-                              has been found 
C-              TRG_SIMUL_EVENT and TRG_SIMUL_RAW_EVENT commons.
C-   
C-
C-   Controls: None.
C-
C-   Created  13-MAR-1990   Sylvain Tisserant (MSU)
C-   Modified 14-Sep-90     Maris Abolins
C-   Updated   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Changed name of routine from L1C_FILL_ONE_EVENT to
C-                          L1_CALTRIG_TOWERS.
C-                        - Replaced call to NORMAL_VARIABLE with call to
C-                          NORRAN.
C-                        - Removed inappropriate reference to Level 0.
C-                        - Moved extraction of ISAJET information to
C-                          routine L1_EXTRACT_ISAJET.
C-                        - Now uses L1SIM_CONTROL.INC to communicate with
C-                          other routines rather than entry points
C-                        - Changed all occurances of
C-                          TRG_SIMUL_ARTIFICIAL_EVENTS to L1C_ART_EVENTS
C-                        - Replaced D0$PARAMS:LEVEL1_CAL_TRIG.PARAMS with
C-                          D0$PARAMS:L1_CALTRIG.PARAMS
C-                        - Replaced D0$PARAMS:LEVEL1_FRAMEWORK.PARAMS with
C-                          D0$PARAMS:L1_FRAMEWORK.PARAMS
C-                        - Replaced D0$INC:LEVEL1_TRIGGER_DATA_BLOCK.INC
C-                          with D0$INC:L1DBB_DATA_BLOCK.INC
C-                        - Replaced D0$INC:TRG_SIMUL_EVENT.INC with
C-                          D0$INC:L1C_EVENT.INC
C-                        - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                          D0$INC:L1C_EVENT_RAW.INC
C-                        - Replaced
C-                          D0$PARAMS:LEVEL1_TRIGGER_DATA_BLOCK.PARAMS with
C-                          D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS
C-   Updated  31-OCT-1991   Philippe Laurens, Steven Klocek
C-                        - Reordered declaration statements to pass D0FLAVOR.
C-   Updated   9-JAN-1992   Philippe Laurens, Steven Klocek
C-                      Replaced variable CRATE_ID with CAD_CRATE_ID to prevent
C-                        conflict with parameter in L1DBB_DATA_BLOCK.PARAMS
C-                        add explicit input bank selection, including new
C-                        option to use TRGR bank for calorimeter input
C-   Updated  27-MAY-1992   James T. Linnemann  use GTCVEV for new calor_util 
C-   Updated  14-JUL-1992   Philippe Laurens, Steven Klocek   
C-                      Consider a missing input source a fatal error on first
C-                      event only, and a warning on subsequent events.
C-                      Add SUCCESS argument so that L1SIM_EVENT may skip.
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      Add code for Trigger Tower Energy Saturation. 
C-                      Fix pointers to CAD banks to use links in HEAD bank and 
C-                        protect from banks moved during garbage collection.
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZCAD1.LINK'
      INCLUDE 'D0$LINKS:IZCAD2.LINK'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      LOGICAL  SUCCESS 
      INTEGER  GZISAE, GZISV1, GZCAD1, GZCAD2, GZCAEP, GZTRGR, TRULEN
      INTEGER  GZFIND_CRATE
      REAL     CAD_GAIN, NORRAN
C
      INTEGER ERR, LUN
      INTEGER ICH ,BITS
      INTEGER MANY_PIECES
      CHARACTER*72 STRING
      INTEGER SLEN
C
      LOGICAL  START
      REAL    NORMAL_VAR
      INTEGER CAD, ADC, ADC_MAX, CHNNL, CHNNL_MAX
      INTEGER BANK_LINK(2), BANK_START 
      INTEGER CAD_CRATE_ID, DATA, PULSHT, SCALE, NEGL
      INTEGER RECORD_TYPE, HEADER_LENGTH, CONTROLLER_WORD, BIT
      INTEGER IETAC, IPHIC, LAYERC, ETA_SIGN, ETA, PHI, CHANNEL
      INTEGER LTRGR_LEVEL1_ADC, LTRGR_LEVEL1
      REAL    CGEV_GAIN 
      LOGICAL EXIST, OK
      REAL    ENERGY
      INTEGER IER
      INTEGER BANK_OFFSET
      INTEGER IUSER
      PARAMETER (IUSER = 537)
C
            LOGICAL EXIST_TRGR, EXIST_CAD1, EXIST_CAD2, EXIST_CAEP
C
      LOGICAL FIRST
      SAVE FIRST
      DATA  FIRST / .TRUE. /
C
C----------------------------------------------------------------------
C
      IF (FIRST) THEN
        FIRST = .FALSE.
C
C       Load the BLS file if requested.
C
        IF (USE_BLS_GAIN_CORRECTION .EQV. .TRUE.) THEN
          CALL GTUNIT(IUSER, LUN, IER)
          IF (IER .NE. 0) THEN
            CALL ERRMSG('GTUNIT','L1_CALTRIG_TOWERS',
     &        'Error calling GTUNIT','F')
            GOTO 999
          ENDIF
          CALL L1C_LOAD_BLS_FILE(LUN)
          CALL RLUNIT(IUSER,LUN,IER)
        ENDIF
C
C       Find the bank to take input from.
C
        EXIST_TRGR = .FALSE.
        IF (GZTRGR() .NE. 0) EXIST_TRGR = .TRUE.
C
        EXIST_CAD1 = .FALSE.
        IF (GZCAD1() .NE. 0) EXIST_CAD1 = .TRUE.
        EXIST_CAD2 = .FALSE.
        IF (GZCAD2() .NE. 0) EXIST_CAD2 = .TRUE.
C
        EXIST_CAEP = .FALSE.
        IF (GZCAEP() .NE. 0) EXIST_CAEP = .TRUE.
C
        STRING = ' '
        IF ( EXIST_TRGR  .EQV. .TRUE.)
     &    STRING = ' TRGR,'
        IF ( EXIST_CAD1  .EQV. .TRUE.)
     &    STRING = STRING(1:TRULEN(STRING)) // ' CAD1,'
        IF ( EXIST_CAD2  .EQV. .TRUE.)
     &    STRING = STRING(1:TRULEN(STRING)) // ' CAD2,'
        IF ( EXIST_CAEP  .EQV. .TRUE.)
     &    STRING = STRING(1:TRULEN(STRING)) // ' CAEP,'
        IF ( STRING .NE. ' ' ) THEN
          SLEN = TRULEN(STRING)
          STRING(SLEN:SLEN) = '.'
          IF ( SLEN .GT. 8 )
     &      STRING = STRING (1:(SLEN-5))
     &          // 'and ' // STRING((SLEN-4):SLEN)
          STRING = ' L1SIM found 1st Event contained bank(s):' // STRING
        ELSE
          STRING = ' L1SIM found 1st Event contained'
     &          // ' no TRGR, CAD nor CAEP banks.'
        END IF
        CALL INTMSG ( STRING )
C
C       Find the bank to use with 'ANY'
C
        IF (L1SIM_INPUT_SOURCE .EQ. 'ANY') THEN
C
          IF       (EXIST_TRGR .EQV. .TRUE.) THEN
            L1SIM_INPUT_SOURCE = 'TRGR'
C
          ELSEIF ( (EXIST_CAD1 .EQV. .TRUE.)
     &      .OR.   (EXIST_CAD2 .EQV. .TRUE.) ) THEN
            L1SIM_INPUT_SOURCE = 'CAD'
C
          ELSEIF   (EXIST_CAEP .EQV. .TRUE.) THEN
            L1SIM_INPUT_SOURCE = 'CAEP'
C
          ENDIF
        ENDIF
C
C       Check that the desired bank exists
C
        IF     ((L1SIM_INPUT_SOURCE .EQ. 'TRGR')
     &    .AND. (EXIST_TRGR .EQV. .TRUE.)) THEN
          CALL INTMSG( ' L1SIM will use TRGR banks'
     &      //         ' for Calorimeter Trigger Input.')
C
        ELSEIF ((L1SIM_INPUT_SOURCE .EQ. 'CAD')
     &          .AND. ((EXIST_CAD1 .EQV. .TRUE.)
     &                 .OR. (EXIST_CAD2 .EQV. .TRUE.)) ) THEN
          CALL INTMSG( ' L1SIM will use CAD1 and CAD2 banks'
     &      //         ' for Calorimeter Trigger Input.')
C
        ELSEIF ((L1SIM_INPUT_SOURCE .EQ. 'CAEP')
     &    .AND. (EXIST_CAEP .EQV. .TRUE.)) THEN
          CALL INTMSG( ' L1SIM will use CAEP banks'
     &      //         ' for Calorimeter Trigger Input.')
C
        ELSEIF (L1SIM_INPUT_SOURCE .EQ. 'NONE') THEN 
          CALL INTMSG( ' L1SIM will not read any bank'
     &      //         ' for Calorimeter Trigger Input.')
C
        ELSE
          STRING = 'No bank appropriate for ' // L1SIM_INPUT_SOURCE
     &      // ' input exists.'
          CALL ERRMSG('NO INPUT BANK', 'L1_CALTRIG_TOWERS', STRING, 'F')
        ENDIF
      ENDIF
C
      RUN_NUMBER   = IQ(LHEAD+6)
      EVENT_NUMBER = IQ(LHEAD+9)
C
      CALL L1_EXTRACT_ISAJET
C
C       Use the artificial events for debugging
C
C       commented out for official release, this is a development tool. The
C       additional code needed is in CMS group NO_DISTRIBUTE
C      IF (LV1_USE_ARTIFICIAL_EVENTS .EQV. .TRUE.) THEN
C        CALL GTUNIT(237, LUN, ERR)
C        IF (ERR .NE. 0) THEN
C          CALL ERRMSG('GTUNIT','L1_CALTRIG_TOWERS',
C     &      'Could not allocate a unit number', 'F')
C          GOTO 999
C        ENDIF
C        CALL L1C_ART_EVENTS (LUN, 'ART_EVENT.DAT',
C     &    1, 0, 0, 4, 'S', .FALSE.)
C        CALL RLUNIT(237, LUN, ERR)
C        GOTO 200
C      ENDIF
C
C
C     Preset energies, with noise generation if requested
C     ===================================================
C
      IF ((L1SIM_INPUT_SOURCE .EQ. 'CAD')
     &  .OR. (L1SIM_INPUT_SOURCE .EQ. 'CAEP')) THEN
C
        TOTAL_CALORIMETRY = 0.
        DO CHANNEL = EM_TOWER, HD_TOWER
          TT_CALORIMETRY (CHANNEL) = 0.
          DO PHI = PHI_MIN, PHI_MAX
            DO ETA = ETA_MIN, ETA_MAX
              DO ETA_SIGN = POS_ETA, NEG_ETA
                IF (APPLY_NOISE) THEN
                  CALL NORRAN(NORMAL_VAR)
                  ENERGY = ELEC_NOISE_SIGMA(ETA_SIGN,ETA,PHI,CHANNEL) *
     +                       NORMAL_VAR
                  TT_ENERGY(ETA_SIGN,ETA,PHI,CHANNEL) = ENERGY
                  TOTAL_CALORIMETRY = TOTAL_CALORIMETRY + ENERGY
                  TT_CALORIMETRY (CHANNEL) = TT_CALORIMETRY (CHANNEL) +
     +                                         ENERGY
                ELSE
                  TT_ENERGY(ETA_SIGN,ETA,PHI,CHANNEL) = 0.
                ENDIF
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      END IF
C
C       Handle Calorimeter input Energy 
C       ===============================
C
      SUCCESS = .TRUE.
C
      IF (L1SIM_INPUT_SOURCE .EQ. 'NONE') THEN
        GOTO 999
C                                                           from CAEP ZEBRA bank
C                                                           --------------------
      ELSE IF (L1SIM_INPUT_SOURCE .EQ. 'CAEP') THEN
        ERR = 0
        START = .TRUE.
        DO WHILE (ERR.EQ.0)
          CALL GTCAEP(START,IETAC,IPHIC,LAYERC,BITS,ENERGY,ICH,ERR)
          START = .FALSE.
          IF(ERR.EQ.-2) THEN
            CALL ERRMSG(' BAD CAEP','L1_CALTRIG_TOWERS',
     &          'Error retrieving data from CAEP bank','W')
            SUCCESS = .FALSE.
            GOTO 999
          ENDIF
          IF(ERR.EQ.0) THEN
            TOTAL_CALORIMETRY = TOTAL_CALORIMETRY + ENERGY
            CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, LAYERC,
     +                                      ETA_SIGN, ETA, PHI, CHANNEL,
     +                                      EXIST)
            IF(EXIST) THEN
              TT_CALORIMETRY(CHANNEL) =
     +            TT_CALORIMETRY(CHANNEL) + ENERGY
              IF (USE_BLS_GAIN_CORRECTION .EQV. .TRUE.) THEN
                TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) =
     +              TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) + ENERGY
     +              * BLS_GAIN_CORRECTION(IETAC, IPHIC, LAYERC)
              ELSE
                TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) =
     +              TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) + ENERGY
              ENDIF
            ENDIF
          ENDIF
        ENDDO
C                                                 from CAD1 and CAD2 ZEBRA banks
C                                                 ------------------------------
      ELSE IF (L1SIM_INPUT_SOURCE .EQ. 'CAD') THEN
        IF ((GZCAD1().EQ.0).AND.(GZCAD2().EQ.0)) THEN
          CALL ERRMSG(' NO CADx BANKS','L1_CALTRIG_TOWERS',
     &           'No CADx or CAEP banks exist for this event.','W')
          SUCCESS = .FALSE.
          GOTO 999
        ENDIF
        BANK_LINK(1)  = IZCAD1
        BANK_LINK(2)  = IZCAD2
        DO 120 CAD = 1, 2
          BANK_START = LQ(LHEAD-BANK_LINK(CAD))
          IF ( BANK_START .EQ. 0 ) GOTO 120
          BANK_OFFSET = 1
          HEADER_LENGTH   = IQ(BANK_START+BANK_OFFSET)
  100     CONTROLLER_WORD = IQ(BANK_START+BANK_OFFSET+2)
          CALL CBYT (CONTROLLER_WORD, THIRD_BYTE,
     +                 ADC_MAX, FIRST_BYTE, BYTE_LENGTH)
          CALL CBYT (CONTROLLER_WORD, FOURTH_BYTE,
     +                 CAD_CRATE_ID, FIRST_BYTE, BYTE_LENGTH)
          BANK_OFFSET = BANK_OFFSET + HEADER_LENGTH + 1
          DO ADC = 0, ADC_MAX
            CHNNL_MAX = IQ(BANK_START+BANK_OFFSET)
            DO 110 CHNNL = 1, CHNNL_MAX
              BANK_OFFSET = BANK_OFFSET + 1
              DATA = IQ(BANK_START+BANK_OFFSET)
              IF(DATA.EQ.0) GOTO 110
              CALL CDTUPK (CAD_CRATE_ID, DATA,
     +                       IETAC, IPHIC, LAYERC, PULSHT, SCALE, NEGL,
     +                       ERR)
              IF (ERR.NE.0) GOTO 110
              ENERGY =FLOAT(PULSHT)*CGEV_GAIN( LAYERC,IPHIC,IETAC,SCALE)
C
C             Restore pointer to beginning of CADn bank to protect against
C             garbage collection triggered in CGEV_GAIN/CGEVFL
              BANK_START = LQ(LHEAD-BANK_LINK(CAD))
C
              TOTAL_CALORIMETRY = TOTAL_CALORIMETRY + ENERGY
              CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, LAYERC,
     +                                    ETA_SIGN, ETA, PHI, CHANNEL,
     +                                    EXIST)
              IF(.NOT.EXIST) GOTO 110
              TT_CALORIMETRY(CHANNEL) =
     +            TT_CALORIMETRY(CHANNEL) + ENERGY
              IF (USE_BLS_GAIN_CORRECTION .EQV. .TRUE.) THEN
                TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) =
     +              TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) + ENERGY
     +              * BLS_GAIN_CORRECTION(IETAC, IPHIC, LAYERC)
              ELSE
                TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) =
     +              TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) + ENERGY
              ENDIF
C
  110       CONTINUE
            BANK_OFFSET = BANK_OFFSET + 1 
          ENDDO
C                                                 Skip the "Crate Trailer Block"
C                                                      (new format only)
          IF(HEADER_LENGTH.GT.3) BANK_OFFSET = BANK_OFFSET + 4
          IF(IQ(BANK_START+BANK_OFFSET).EQ.HEADER_LENGTH) GOTO 100
  120   CONTINUE
C
C                                                           from TRGR ZEBRA bank
C                                                           --------------------
C
      ELSE IF (L1SIM_INPUT_SOURCE .EQ. 'TRGR') THEN
C
        LTRGR_LEVEL1 = GZFIND_CRATE('TRGR', GZTRGR(), CRATE_ID)
        IF (LTRGR_LEVEL1 .LE. 0) THEN
          CALL ERRMSG('NO LEVEL 1 CRATE', 'L1_CALTRIG_TOWERS',
     &      'No Level 1 Information for this event.', 'W')
          SUCCESS = .FALSE.
          GOTO 999
        ENDIF
        LTRGR_LEVEL1_ADC = LTRGR_LEVEL1 + TRGR_HEADER_LENGTH + 1
     &    + (TT_FADC-1)/2
C
        CALL L1UTIL_ADC_COUNT_UNPACK(IQ(LTRGR_LEVEL1_ADC), FADC_BYTE)
C
      ENDIF
C
C
C     FADC simulation for Calorimeter Banks
C     ===========================
C
  200 CONTINUE
      IF    ((L1SIM_INPUT_SOURCE .EQ. 'CAD')
     &  .OR. (L1SIM_INPUT_SOURCE .EQ. 'CAEP')) THEN
C
C       Provide energy saturation
        DO CHANNEL = EM_TOWER, HD_TOWER
          DO PHI = PHI_MIN, PHI_MAX
            DO ETA = ETA_MIN, ETA_MAX
              DO ETA_SIGN = POS_ETA, NEG_ETA
                TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL) 
     &            = MIN(TT_ENERGY(ETA_SIGN, ETA, PHI, CHANNEL),
     &                  TRIGGER_TOWER_SATURATION(CHANNEL, ETA))
              END DO
            END DO
          END DO
        END DO
C
        CALL ADC_CONVERSIONS (TT_ENERGY, FADC_BYTE)
      ENDIF
C
C     Count the hit Trigger Towers
C     ============================
C
      DO CHANNEL = EM_TOWER, HD_TOWER
        HIT_TRIGGER_TOWER(CHANNEL) = 0
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DO ETA_SIGN = POS_ETA, NEG_ETA
C
              IF(FADC_BYTE(ETA_SIGN,ETA,PHI,CHANNEL).GT.
     +            ADC_ZERESP(ETA_SIGN,ETA,PHI,CHANNEL)  )
     +           HIT_TRIGGER_TOWER(CHANNEL) =
     +              HIT_TRIGGER_TOWER(CHANNEL) + 1
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C
  999 RETURN
      END
