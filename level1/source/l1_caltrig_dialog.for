      SUBROUTINE L1_CALTRIG_DIALOG ( )
C--------------------------------------------------------------------
C-
C-   Purpose and Methods: Engages user in dialogue to get run
C-                        parameters at run time.
C-
C-   Inputs  : Common block variables
C-
C-   Outputs : Common block variables
C-
C-   Controls: None.
C-
C-   Created  27-APR-1990   Sylvain Tisserant (MSU)
C-   MODIFIED 13-Sep-1990   Maris Abolins (MSU)
C-   Revised 27-Nov-90      Maris Abolins - fix inconsistencies with
C-   default responses
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Now prompts for all boolean parameters in the
C-                            initial RCP file. 
C-                          - Changed from a function to a subroutine.
C-                          - Now uses L1SIM_CONTROL.INC to communicate with
C-                            other routines rather than entry points.
C-                          - Changed name of routine from L1C_DIAL to
C-                            L1_CALTRIG_DIALOG. 
C-                          - Cosmetic changes to pass D0FLAVOR
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek   
C-                          Now prompts for value of APPLY_PRESCALER.
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek  
C-                      Fixed problem when PATH is prompted for and the user
C-                      selects BACK.
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      All logical values are obtained using the routine
C-                      L1UTIL_GET_PARAM_DIALOG. 
C-   Updated   3-MAR-1992   Philippe Laurens, Steven Klocek  
C-                      add Force L0 Vertex and, input bank selection
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      add copy L0 Crate 
C-                      Add switch to create the Jet Lists.
C-   Updated   4-MAR-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch PRESCALER_USE_EVENT_DATA 
C-
C----------------------------------------------------------------------
C
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
C
      CHARACTER*4  SPATH, BANK
C
      CHARACTER*80 LINE
      INTEGER TRULEN
      EXTERNAL TRULEN
C
C
C----------------------------------------------------------------------
C
C                                                                Initialization
C                                                                --------------
C       SET DEFAULT SWITCH SETTINGS
C
C       Choose bank from which to take input for ADC bytes
C
      CALL INTMSG(
     &  ' Possible choices for input bank: ANY, TRGR, CAD, CAEP, NONE')
      CALL INTMSG(
     &  ' ''ANY'' implies a search order of: TRGR, CAD, CAEP')
      BANK = L1SIM_INPUT_SOURCE
      LINE = 'Which bank shoud be used as input to L1SIM? ['
     &       // BANK
     &       // ']:>'
      CALL GETPAR(1, LINE(1:TRULEN(LINE)), 'U', BANK)
      IF (BANK .NE. ' ') THEN
        IF ((BANK .EQ. 'ANY') 
     &    .OR. (BANK .EQ. 'CAEP' )
     &    .OR. (BANK .EQ. 'TRGR' )
     &    .OR. (BANK .EQ. 'CAD'  )
     &    .OR. (BANK .EQ. 'NONE' )) THEN
          L1SIM_INPUT_SOURCE = BANK
        ENDIF
      ENDIF
      LINE = ' ' // L1SIM_INPUT_SOURCE 
     &       // ' bank(s) will be used as input to L1SIM'
      CALL INTMSG(LINE)
C
C       Add noise to trigger towers
C
      CALL L1UTIL_GET_PARAM_DIALOG(APPLY_NOISE,
     &  'Add Noise to Energy From Calorimeter Banks?',
     &  'Noise will be added to Energy From Calorimeter Banks',
     &  'Noise will not be added to Energy From Calorimeter Banks')
C
C       Use or Ignore Level 0 Vertex
C
      CALL L1UTIL_GET_PARAM_DIALOG( FORCE_VERTEX_CENTER,
     &  'Ignore Level 0 Vertex for computing Level 1 Lookup Page?',
     &  'Level 0 Information will be ignored',
     &  'Level 0 Information will be used')
C
C       Copy Level 0 Crate
C
      CALL L1UTIL_GET_PARAM_DIALOG( COPY_L0_CRATE,
     &  'Copy L0 Crate from input event to output event?',
     &  'Level 0 Crate will be copied',
     &  'Level 0 Crate will not be copied' )
C
C       Create EM Et and Tot Et Jet Lists
C
      CALL L1UTIL_GET_PARAM_DIALOG( CREATE_JET_LISTS, 
     &  'Write EM and Tot Jet Lists to output event?',
     &  'Jet Lists will be created',
     &  'Jet Lists will not be created' )
C
C       Use Fast Calorimeter algorithm
C
      CALL L1UTIL_GET_PARAM_DIALOG(DO_FAST_CALTRIG,
     &  'Perform Fast Trigger Simulation?',
     &  'Fast simulation will be done; no TRGR bank created',
     &  'Normal simulation will be performed ')
C
C       Apply Prescalers
C
      CALL L1UTIL_GET_PARAM_DIALOG(APPLY_PRESCALER,
     &  'Apply Prescaling Ratios?',
     &  'Prescaling Ratios will be applied.',
     &  'Prescaling Ratios will be ignored.')
C

      IF (APPLY_PRESCALER .EQV. .TRUE.) 
     & CALL L1UTIL_GET_PARAM_DIALOG(PRESCALER_USE_EVENT_DATA,
     &  'Use input event data for prescaler final decision?',
     &  'Prescaling Mode will follow input event data.',
     &  'Prescaling Mode will follow programmed Ratios.')
C
C       Get state of USE_BLS_GAIN_CORRECTION
C
      CALL L1UTIL_GET_PARAM_DIALOG(USE_BLS_GAIN_CORRECTION,
     &  'Use BLS Gain Correction?',
     &  'BLS Gain Correction will be used.',
     &  'BLS Gain Correction will not be used.')
      IF (USE_BLS_GAIN_CORRECTION .EQV. .TRUE.) THEN
        LINE = BLS_FILE_NAME(1:BLS_FILE_NAME_LENGTH)
        CALL INTMSG(' Default BLS Gain Correction File: ' // 
     &    LINE)
        CALL GETPAR(1,'BLS Gain Correction File Name:', 'U', 
     &    LINE)
        IF (LINE .NE. ' ') THEN
          BLS_FILE_NAME = LINE
          BLS_FILE_NAME_LENGTH = TRULEN(BLS_FILE_NAME)
        ENDIF
        CALL INTMSG(' New BLS Gain Correction File: ' // 
     &    BLS_FILE_NAME(1:BLS_FILE_NAME_LENGTH))
      ENDIF
C
C       Programming listing in the standard summary
C
      CALL L1UTIL_GET_PARAM_DIALOG(DO_PROGRAMMING_LISTING,
     &  'List programming information in summary file?',
     &  'Programming listing will be performed',
     &  'Programming listing will not be performed')
C
C       NOW GET PATH
C
      SPATH = ZEBRA_PATH
      WRITE (LINE,9003) ZEBRA_PATH
 9003 FORMAT('Chose PATH (RECO, FAKE or GEAN), [',A4,']:>')
C
      CALL GETPAR(1, LINE(1:42),'U',SPATH)
      IF (SPATH.NE.' ') ZEBRA_PATH = SPATH 
      CALL PATHDF (ZEBRA_PATH )
      CALL PATHRS
      WRITE (LINE,8003) ZEBRA_PATH
 8003 FORMAT(' Default PATH is now ',A4,'.')
      CALL INTMSG (LINE(1:30))
C
      RETURN
      END
