      LOGICAL FUNCTION L1UTIL_DISP_TOWER_ENERGY()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the energy is each depth for a particular
C-    Trigger Tower
C-
C-   Inputs  : Event data
C-   Outputs : screen output
C-   Controls: none
C-
C-   Created  26-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                            - Removed reference to NORMAL_VARIABLE
C-                            - Changed function definition to pass D0FLAVOR.
C-                              Reordered declaration statements to pass
C-                              D0FLAVOR
C-   Updated   9-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      Added definitions for FIRST_BYTE,... so that
C-                        D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS would not be
C-                        necessary. Several parameters moved to this file
C-                        conflicted with variables in this routine. Copied 
C-                        the definition of the needed parameters into this
C-                        routine.
C-   Updated  27-MAY-1992   James T. Linnemann  use GTCGEV for new calor_util 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PI.DEF'
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$INC:QUEST.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$LINKS:IZISP1.LINK'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
C
      INTEGER    BYTE_LENGTH,     WORD_LENGTH,
     +           LONG_WORD_LENGTH
      PARAMETER (BYTE_LENGTH      = 8,
     +           WORD_LENGTH      = 2 * BYTE_LENGTH,
     +           LONG_WORD_LENGTH = 4 * BYTE_LENGTH)
C
      INTEGER    FIRST_BYTE,   SECOND_BYTE,
     +           THIRD_BYTE,   FOURTH_BYTE
      PARAMETER (FIRST_BYTE  = 1,
     +           SECOND_BYTE = FIRST_BYTE  + BYTE_LENGTH,
     +           THIRD_BYTE  = SECOND_BYTE + BYTE_LENGTH,
     +           FOURTH_BYTE = THIRD_BYTE  + BYTE_LENGTH )
C
      INTEGER  GZCAEP, GZCAD1, GZCAD2, TRULEN
      REAL     CAD_GAIN
C
      INTEGER ERR, LUN
      INTEGER ICH ,BITS
      INTEGER MANY_PIECES
      CHARACTER*79 STRING
      INTEGER SLEN
      LOGICAL NOISE, NOISE_IN
      LOGICAL USE_BLS_GAIN_CORRECTION, USE_BLS_IN
      SAVE NOISE
C
      LOGICAL  START
      INTEGER I, LSV1, PNTR, PARTICLE
      INTEGER CAD, ADC, ADC_MAX, CHNNL, CHNNL_MAX
      INTEGER BANK_START(2), CRATE_ID, DATA, PULSHT, SCALE, NEGL
      INTEGER RECORD_TYPE, HEADER_LENGTH, CONTROLLER_WORD, BIT
      INTEGER IETAC, IPHIC, ILYRC, ETA_SIGN, ETA, PHI, CHANNEL
      INTEGER ETA2, PHI2, CHANNEL2, ETA_SIGN2
      REAL    CGEV_GAIN
      LOGICAL EXIST, OK
      LOGICAL WRITTEN
      REAL    ENERGY
      INTEGER IER
      INTEGER IUSER
      PARAMETER (IUSER = 537)
C
      LOGICAL FIRST
      SAVE FIRST
      DATA  FIRST / .TRUE. /
C
C----------------------------------------------------------------------
C
      L1UTIL_DISP_TOWER_ENERGY = .TRUE.
      IF (FIRST) THEN
C
C       Initialization for Calorimeter routines we use
        CALL CHTINI()
        FIRST = .FALSE.
C
C       DAC byte must be non-zero to exist. Initialize here since we don't use
C         the LSM library.
        DO CHANNEL = EM_TOWER, HD_TOWER
          DO PHI = PHI_MIN, PHI_MAX
            DO ETA = ETA_MIN, ETA_MAX
              DAC_BYTE(POS_ETA,ETA, PHI, CHANNEL) = 1
              DAC_BYTE(NEG_ETA,ETA, PHI, CHANNEL) = 1
            END DO
          END DO  
        END DO
      ENDIF
C
      RUN_NUMBER   = IQ(LHEAD+6)
      EVENT_NUMBER = IQ(LHEAD+9)
      WRITE (STRING, 300) RUN_NUMBER, EVENT_NUMBER
  300 FORMAT (' Run number:', I10, ' Event number:', I10)
      CALL OUTMSG(' ')
      CALL OUTMSG(STRING)
C
C       Get Coordinates to look at
 3000 CONTINUE
      CALL OUTMSG(' <ENTER> to proceed to next event')
      ETA = 0
      CALL GETPAR(1, 'ETA Coordinate >', 'I', ETA)
      IF (ETA .EQ. 0) GOTO 999
      ETA_SIGN = POS_ETA
      IF (ETA .LT. 0) THEN
        ETA = -ETA
        ETA_SIGN = NEG_ETA
      ENDIF
C
      CALL GETPAR(1, 'PHI Coordinate >', 'I', PHI)
      IF (PHI .EQ. 0) GOTO 999
      CALL OUTMSG(' ')
C
C     Calorimeter data
C     ================
C                                                           from CAEP ZEBRA bank
C                                                           --------------------
      WRITTEN = .FALSE.
      IF(GZCAEP().NE.0) THEN
        ERR = 0
        START = .TRUE.
        DO WHILE (ERR.EQ.0)
          CALL GTCAEP(START,IETAC,IPHIC,ILYRC,BITS,ENERGY,ICH,ERR)
          START = .FALSE.
          IF(ERR.EQ.-2) THEN
            CALL ERRMSG(' BAD CAEP','L1UTIL_DISP_TOWER_ENERGY',
     &        'Error retrieving data from CAEP bank','F')
            ERR = 2
            GOTO 999
          ENDIF
          IF(ERR.EQ.0) THEN
            TOTAL_CALORIMETRY = TOTAL_CALORIMETRY + ENERGY
            CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, ILYRC,
     +                                    ETA_SIGN2, ETA2, PHI2, 
     +                                    CHANNEL2, EXIST)
C
C       If the tower exists and the coordinates match the selection, then print
C       out the energy.
            IF(EXIST) THEN
              IF ((ETA_SIGN .EQ. ETA_SIGN2)
     &          .AND. (ETA .EQ. ETA2)
     &          .AND. (PHI .EQ. PHI2)) THEN
                IF (WRITTEN .EQV. .FALSE.) THEN
                  IF (ETA_SIGN .EQ. POS_ETA) THEN
                    WRITE (STRING, 310) ETA, PHI
                  ELSE
                    WRITE (STRING,310) -ETA, PHI
                  ENDIF
  310             FORMAT ( ' ETA=',I3,' PHI=',I2, ' From CAEP Bank')
                  CALL OUTMSG(STRING)
                  CALL OUTMSG( 
     & ' IETA IPHI ILYR Deposited energy') 
                  CALL OUTMSG(
     & ' -------------------------------')
                  WRITTEN = .TRUE.
                ENDIF
C
                WRITE (STRING,320) IETAC, IPHIC, ILYRC, ENERGY
  320           FORMAT(' ', I3, X, I3, X, I3, 4X, F12.7)
                CALL OUTMSG(STRING)
              ENDIF
C
            ENDIF
          ENDIF
        ENDDO
        IF (WRITTEN .EQV. .FALSE.) THEN
          CALL OUTMSG(
     &      ' No energy deposits exist for that Trigger Tower')
        ENDIF
      ELSE
C                                                 from CAD1 and CAD2 ZEBRA banks
C                                                 ------------------------------
        WRITTEN = .FALSE.
        BANK_START(1) = GZCAD1()
        BANK_START(2) = GZCAD2()
        IF ((BANK_START(1).EQ.0).AND.(BANK_START(2).EQ.0)) THEN
          CALL ERRMSG(' NO CADx BANKS','L1UTIL_DISP_TOWER_ENERGY',
     &      'No CADx or CAEP banks exist for this event.','F')
          ERR = 3
          GOTO 999
        ENDIF
        DO 120 CAD = 1, 2
          PNTR = BANK_START(CAD)
          IF(PNTR.EQ.0) GOTO 120
          PNTR = PNTR + 1
          HEADER_LENGTH   = IQ(PNTR)
  100     CONTROLLER_WORD = IQ(PNTR+2)
          CALL CBYT (CONTROLLER_WORD, THIRD_BYTE,
     +               ADC_MAX, FIRST_BYTE, BYTE_LENGTH)
          CALL CBYT (CONTROLLER_WORD, FOURTH_BYTE,
     +               CRATE_ID, FIRST_BYTE, BYTE_LENGTH)
          PNTR = PNTR + HEADER_LENGTH + 1
          DO ADC = 0, ADC_MAX
            CHNNL_MAX = IQ(PNTR)
            DO 110 CHNNL = 1, CHNNL_MAX
              PNTR = PNTR + 1
              DATA = IQ(PNTR)
              IF(DATA.EQ.0) GOTO 110
              CALL CDTUPK (CRATE_ID, DATA,
     +                     IETAC, IPHIC, ILYRC, PULSHT, SCALE, NEGL,
     +                     ERR)
              IF (ERR.NE.0) GOTO 110
              ENERGY =FLOAT(PULSHT)*CGEV_GAIN( ILYRC,IPHIC,IETAC,SCALE)
              TOTAL_CALORIMETRY = TOTAL_CALORIMETRY + ENERGY
              CALL WHICH_TRIGGER_TOWER (IETAC, IPHIC, ILYRC,
     +                                  ETA_SIGN2, ETA2, PHI2, CHANNEL2,
     +                                  EXIST)
C
C       If the tower exists and the coordinates match the selection, then print
C       the energy.
              IF(.NOT.EXIST) GOTO 110
              IF ((ETA_SIGN .EQ. ETA_SIGN2)
     &          .AND. (ETA .EQ. ETA2)
     &          .AND. (PHI .EQ. PHI2)) THEN
                IF (WRITTEN .EQV. .FALSE.) THEN
                  IF (ETA_SIGN .EQ. POS_ETA) THEN
                    WRITE (STRING, 330) ETA, PHI
                  ELSE
                    WRITE (STRING,330) -ETA, PHI
                  ENDIF
  330             FORMAT ( ' ETA=',I3,' PHI=',I2, ' From CADx Banks')
                  CALL OUTMSG(STRING)
                  CALL OUTMSG( 
     & ' IETA IPHI ILYR Deposited energy') 
                  CALL OUTMSG(
     & ' -------------------------------')
                  WRITTEN = .TRUE.
                ENDIF
C
                WRITE (STRING,340) IETAC, IPHIC, ILYRC, ENERGY
  340           FORMAT(' ', I3, X, I3, X, I3, 4X, F12.7)
                CALL OUTMSG(STRING)
              ENDIF
C
  110       CONTINUE
            PNTR = PNTR + 1
          ENDDO
C                                                 Skip the "Crate Trailer Block"
C                                                      (new format only)
          IF(HEADER_LENGTH.GT.3) PNTR = PNTR + 4
          IF(IQ(PNTR).EQ.HEADER_LENGTH) GOTO 100
  120   CONTINUE
C
        IF (WRITTEN .EQV. .FALSE.) THEN
          CALL OUTMSG(
     &      ' No energy deposits exist for that Trigger Tower')
        ENDIF
C
      ENDIF
      GOTO 3000
C
C
C
C----------------------------------------------------------------------
  999 RETURN
      END
