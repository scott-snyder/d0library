      PROGRAM L1UTIL_DISP_BLS_COEFF
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Display the BLS Gain Correction coefficients for a
C-    particular trigger tower.
C-
C-
C-   Inputs:   NONE
C-
C-   Outputs : NONE
C-
C-   Controls: None.
C-
C-   Created 25-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Modified to get the name of the RCP file from
C-                            L1SIM_CONTROL.PARAMS 
C----------------------------------------------------------------------
C
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:CAL_OFFLINE.PARAMS'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1C_BLS_GAIN_CORRECTION.INC'
C
      INTEGER TRULEN
      EXTERNAL TRULEN
      INTEGER ERR, LUN
      INTEGER COUNT
      CHARACTER*72 STRING
      INTEGER SLEN
C
      INTEGER IETAC, IPHIC, ILYRC, SIGN_ETA, ETA, PHI, CHANNEL
      INTEGER SIGN_ETA2, ETA2, PHI2, CHANNEL2
      LOGICAL EXIST, OK
      LOGICAL WRITTEN
      INTEGER IER
      INTEGER IUSER
      PARAMETER (IUSER = 537)
      INTEGER RLEN
      PARAMETER (RLEN = 16)
      CHARACTER*1 TYPAR(RLEN)
      INTEGER VALAR(RLEN)
C
C----------------------------------------------------------------------
C
C       Initialization of parameters used in creating RCP entries
      DO COUNT = 1, RLEN
        TYPAR(COUNT) = 'H'
        VALAR(COUNT) = 0
      END DO
C
C       DAC_BYTE must be non-zero for the tower to exist. Since LSM library is
C       not used in this application, initialize here.
      DO CHANNEL = EM_TOWER, HD_TOWER
        DO PHI = PHI_MIN, PHI_MAX
          DO ETA = ETA_MIN, ETA_MAX
            DAC_BYTE(POS_ETA,ETA, PHI, CHANNEL) = 1
            DAC_BYTE(NEG_ETA,ETA, PHI, CHANNEL) = 1
          END DO
        END DO  
      END DO
C
      CALL MZEBRA(0)
      CALL SETCHK
      CALL INZSTP
C
C       Get file name, create RCP bank, and store file name in bank
      CALL GETPAR(1,'Enter BLS file name>','U',STRING)
      CALL EZBOOK(L1SIM_RCPFILE, 30, 20, 20)
      CALL EZPICK(L1SIM_RCPFILE)
      CALL EZFILL('BLS_GAIN_CORRECTION_FILE',' ',VALAR,TYPAR,RLEN)
      CALL EZERR(IER)
      CALL EZEND
      CALL EZERR(IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('EZSETS','L1UTIL_DISP_BLS_COEFF',
     &    'Error adding BLS_GAIN_CORRECTION_FILE','F')
      ENDIF
      CALL EZSETS('BLS_GAIN_CORRECTION_FILE',1,STRING,TRULEN(STRING),
     &  IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('EZSETS','L1UTIL_DISP_BLS_COEFF',
     &    'Error setting BLS_GAIN_CORRECTION_FILE','F')
      ENDIF
      CALL EZRSET
C
C       Load the BLS file
      CALL GTUNIT(IUSER, LUN, IER)
      IF (IER .NE. 0) THEN
        CALL ERRMSG('GTUNIT','L1UTIL_DISP_BLS_COEFF',
     &    'Error calling GTUNIT','F')
        GOTO 999
      ENDIF
      CALL L1C_LOAD_BLS_FILE(LUN)
      CALL RLUNIT(IUSER,LUN,IER)
C
      CALL OUTMSG(' BLS File Loaded')
C
C       Get the coordinates to look at
 1000 CONTINUE
      ETA = 0
      CALL GETPAR(1,' Trigger Tower ETA >', 'I', ETA)
      IF (ETA .EQ. 0) GOTO 999 
      SIGN_ETA = POS_ETA
      IF (ETA .LT. 0) THEN
        ETA = -ETA
        SIGN_ETA = NEG_ETA
      ENDIF
      PHI = 0
      CALL GETPAR(1, ' Trigger Tower PHI >', 'I', PHI)
      IF (PHI .EQ. 0) GOTO 999
C
C       Loop through all possible coordinates and print out those that match
C       the selected coordinates.
      WRITTEN = .FALSE.
      DO ILYRC = 1, NLYRL
        DO IPHIC = 1, NPHIL
          DO IETAC = -NETAL, NETAL
            CALL WHICH_TRIGGER_TOWER(IETAC, IPHIC, ILYRC, 
     &        SIGN_ETA2, ETA2, PHI2, CHANNEL2, EXIST)
            IF (EXIST .EQV. .TRUE.) THEN
              IF ((SIGN_ETA2 .EQ. SIGN_ETA) 
     &          .AND. (ETA2 .EQ. ETA) 
     &          .AND. (PHI2 .EQ. PHI)) THEN
                IF (WRITTEN .EQV. .FALSE.) THEN
                  IF (SIGN_ETA .EQ. POS_ETA) THEN
                    WRITE (STRING, 50) ETA, PHI
                  ELSE
                    WRITE (STRING,50) -ETA, PHI
                  ENDIF
   50             FORMAT (' ETA=',I3,' PHI=',I2)
                  CALL OUTMSG('  ')
                  CALL OUTMSG(STRING)
                  CALL OUTMSG(' IETA IPHI ILYR BLS Gain Coefficient')
                  CALL OUTMSG(' -----------------------------------')
                  WRITTEN = .TRUE.
                ENDIF
C
                WRITE (STRING,100) IETAC, IPHIC, ILYRC, 
     &            BLS_GAIN_CORRECTION(IETAC, IPHIC, ILYRC)
  100           FORMAT(' ', I3, X, I3, X, I3, 4X, F12.9)
                CALL OUTMSG(STRING)
              ENDIF
            ENDIF
          END DO
        END DO
      END DO
C
      IF (WRITTEN .NEQV. .TRUE.) THEN
        CALL OUTMSG(' No Depths exist for that Tower.')
      ENDIF
      GOTO 1000
C
C----------------------------------------------------------------------
  999 CONTINUE
      CALL OUTMSG('  ')
      CALL EXIMEN(1,1)
      END
