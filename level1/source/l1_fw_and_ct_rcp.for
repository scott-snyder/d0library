      SUBROUTINE L1_FW_AND_CT_RCP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Read the RCP file L1SIM_RCP, and extract the
C-      necessary parameters from it.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Retrieves the value of APPLY_PRESCALER_DEFAULT.
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      renamed from L1_READ_L1SIM_RCP to L1_FW_AND_CT_RCP
C-                      move initialization of CAHITS, and read-in of RCP to 
C-                      L1SIM_INI.FOR
C-   Updated   4-FEB-1992   Philippe Laurens, Steven Klocek  
C-                      Moved display of Programming file name to routine which
C-                        actually loads the programming file.
C-                      Added L1SIM_INPUT_SOURCE parameter, defines which bank
C-                        ADC input comes from.
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      Added code to unpack Forced Andor Terms and Trigger
C-                      Tower Energy Saturation. 
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Read array of names of andor terms copied from an
C-                      existing TRGR bank found in the input event
C-                      Read switch to copy L0 crate
C-                      Read switch to create the Jet Lists.
C-   Updated   4-MAR-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch PRESCALER_USE_EVENT_DATA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      CHARACTER*72 STRING
C
      INTEGER LENF
C
      INTEGER      IERR, IER
      INTEGER      WORD_COUNT, RCP_ENTRY, NAME_LENGTH
      CHARACTER*32 ANDOR_TERM_NAME
C
      INTEGER      TOWER, ETA, LENGTH
      REAL         BIG_ENERGY
      PARAMETER   (BIG_ENERGY = 1000.)
C
C
C----------------------------------------------------------------------
C
C       Pick up RCP parameters
      CALL L1UTIL_PICK_L1SIM_RCP
C
C***** Get name of LOOKUP_TABLE_FILE
      CALL EZGETS (LOOKUP_FILE_RCPKEY,1,LOOKUP_TABLE_FILE_NAME, 
     &  LOOKUP_TABLE_FILE_NAME_LENGTH, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (LOOKUP_FILE_RCPKEY, 'L1_FW_AND_CT_RCP', 
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C******* Read in Configuration File name
C
      CALL EZGETS (CONFIG_FILE_RCPKEY,1,PROGRAMMING_FILE_NAME,
     &  PROGRAMMING_FILE_NAME_LENGTH, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (CONFIG_FILE_RCPKEY, 'L1_FW_AND_CT_RCP', 
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Get resource file name
C
      CALL EZGETS(RESOURCE_FILE_RCPKEY,1, RESOURCE_FILE_NAME,
     &  RESOURCE_FILE_NAME_LENGTH, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (RESOURCE_FILE_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Get name of BLS Gain Correction file
C
      CALL EZGETS(BLS_FILE_RCPKEY, 1, BLS_FILE_NAME,
     &  BLS_FILE_NAME_LENGTH, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG(BLS_FILE_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING, 'F')
        GOTO 2000
      ENDIF
C
C       Now pick up other needed parameters
C
C       start with PATH to CAEP banks
C
      CALL EZGETS (PATH_RCPKEY,1,ZEBRA_PATH,LENF,IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG(PATH_RCPKEY,'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       now go to pick up list control parameters
C
      CALL EZGET (PROGRAMMING_LISTING_RCPKEY, 
     &  DO_PROGRAMMING_LISTING, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (PROGRAMMING_LISTING_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Now pick up remaining parameters
C
C       Fast simulation
      CALL EZGET (FAST_CALTRIG_RCPKEY , DO_FAST_CALTRIG ,IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (FAST_CALTRIG_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Add noise
      CALL EZGET (NOISE_RCPKEY , APPLY_NOISE ,IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (NOISE_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Use BLS Gain Correction coefficient 
      CALL EZGET(USE_BLS_DEFAULT_RCPKEY, USE_BLS_GAIN_CORRECTION, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (USE_BLS_DEFAULT_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING,'F')
        GOTO 2000
      ENDIF
C
C       Apply prescalers
      CALL EZGET(APPLY_PRESCALER_RCPKEY, APPLY_PRESCALER, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER,STRING)
        CALL ERRMSG (APPLY_PRESCALER_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING, 'F')
        GOTO 2000
      ELSE
        IF (APPLY_PRESCALER .EQV. .TRUE.) THEN
          CALL EZGET( PRESCALER_USE_EVENT_DATA_RCPKEY,
     &                PRESCALER_USE_EVENT_DATA, IER)
          IF (IER .NE. 0) PRESCALER_USE_EVENT_DATA = .FALSE.
        ENDIF
      ENDIF
C
C       Force the center Lookup Page
C
      CALL EZGET(FORCE_VERTEX_CENTER_RCPKEY, FORCE_VERTEX_CENTER, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG(FORCE_VERTEX_CENTER_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING, 'F')
        GOTO 2000                     
      ENDIF
C
C       Copy L0 Crate
C
      CALL EZGET( COPY_L0_CRATE_RCPKEY, COPY_L0_CRATE, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG( COPY_L0_CRATE_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING, 'F')
        GOTO 2000                     
      ENDIF
C
      CALL EZGET( CREATE_JET_LISTS_RCPKEY, CREATE_JET_LISTS, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG( CREATE_JET_LISTS_RCPKEY, 'L1_FW_AND_CT_RCP',
     &    STRING, 'F')
        GOTO 2000                     
      ENDIF
C
C       ZEBRA bank for input to Level 1 Caltrig ADC
C
      CALL EZGETS(L1SIM_INPUT_SOURCE_RCPKEY, 1, L1SIM_INPUT_SOURCE, 
     &  LENF, IER)
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG(L1SIM_INPUT_SOURCE_RCPKEY,
     &              'L1_READ_L1SIM_RCP', 
     &              STRING, 'F')
        GOTO 2000
      ENDIF
C
      CALL PATHDF (ZEBRA_PATH )
      CALL PATHRS
      WRITE (STRING,8003) ZEBRA_PATH
 8003 FORMAT(' Default PATH is now ',A4,'.')
      CALL INTMSG (STRING(1:30))
C
C       Trigger Tower saturation energies
C
      DO ETA = ETA_MIN, ETA_MAX
        DO TOWER = EM_TOWER, HD_TOWER
          TRIGGER_TOWER_SATURATION(TOWER, ETA) = BIG_ENERGY
        END DO
      END DO
      CALL EZGETA( TRIGGER_TOWER_SATURATION_RCPKEY, 0, 0, 1, LENGTH,
     &  IER)
      IF ((IER .EQ. 0) .AND. (LENGTH .GT. 0)) THEN
        CALL EZGETA( TRIGGER_TOWER_SATURATION_RCPKEY, 1, LENGTH, 1,
     &    TRIGGER_TOWER_SATURATION, IER)
      ENDIF
C
C       Load Forced Term Logical Names and States
C
      WORD_COUNT = 0
      NUM_FORCED_TERMS = 0
      RCP_ENTRY = 0
C
      DO WHILE (.TRUE.)
C
        NAME_LENGTH = 0
        RCP_ENTRY  = RCP_ENTRY + 1
        CALL EZGETS(FORCED_ANDOR_TERMS_RCPKEY, RCP_ENTRY, 
     &      ANDOR_TERM_NAME, NAME_LENGTH, IER)
C
        IF ((NAME_LENGTH .EQ. 0) .OR. (IER .NE. 0)) 
     &  THEN
          IF (IER .NE. 0) 
     &    THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG( 'FORCED ANDOR NAME', 'L1_FW_AND_CT_RCP',
     &                   STRING, 'W')
          END IF
          GOTO 200
        END IF
C
C       Find location of boolean flag for andor term state
        WORD_COUNT = WORD_COUNT + (NAME_LENGTH +3)/4 + 1
C
        IF ( ANDOR_TERM_NAME .NE. ' ' ) 
     &  THEN 
          NUM_FORCED_TERMS = NUM_FORCED_TERMS+1
          CALL EZGETA( FORCED_ANDOR_TERMS_RCPKEY, WORD_COUNT, 
     &         WORD_COUNT, 1, FORCED_TERM_STATES(NUM_FORCED_TERMS), IER)
          FORCED_TERM_NAMES(NUM_FORCED_TERMS) 
     &        = ANDOR_TERM_NAME(1:NAME_LENGTH)
        END IF
C        
      END DO
  200 CONTINUE
C
C       Load Copied Andor Term Logical Names 
C
      NUM_COPIED_TERMS = 0
      RCP_ENTRY = 0
      DO WHILE (.TRUE.)
C
        NAME_LENGTH = 0
        RCP_ENTRY  = RCP_ENTRY + 1
        CALL EZGETS( COPIED_ANDOR_TERMS_RCPKEY, RCP_ENTRY,
     &               ANDOR_TERM_NAME, NAME_LENGTH, IER)
C
        IF ((NAME_LENGTH .EQ. 0) .OR. (IER .NE. 0)) 
     &  THEN 
          IF (IER .NE. 0) 
     &    THEN
            CALL EZGET_ERROR_TEXT(IER,STRING)
            CALL ERRMSG( 'COPIED ANDOR NAME', 'L1_FW_AND_CT_RCP',
     &                   STRING, 'W')
          END IF
          GOTO 300
        END IF
C
        IF ( ANDOR_TERM_NAME .NE. ' ' ) 
     &  THEN 
          NUM_COPIED_TERMS = NUM_COPIED_TERMS +1
          COPIED_TERM_NAMES(NUM_COPIED_TERMS) 
     &      = ANDOR_TERM_NAME(1:NAME_LENGTH)
        END IF
C
      END DO
  300 CONTINUE
C
C       De-select RCP bank
C
 2000 CONTINUE
      CALL EZRSET
C
  999 RETURN
C
      END
