      SUBROUTINE L1_AND_L15_CONTROL_SSUM(LUN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the values of variables from L1SIM_CONTROL
C-   used by the Level 1 CalTrig and Framework and by the Level 1.5 Framework.
C-
C-   Inputs  : LUN      The logical unit number to write to.
C-             Common block L1SIM_CONTROL
C-   Outputs : Standard Summary file and Event Dump file
C-   Controls: none
C-
C-   Created  17-DEC-1991   Philippe Laurens, Steven Klocek
C-   Updated  30-JAN-1992   Philippe Laurens, Steven Klocek  
C-                      Fixed mispelled message.
C-                      Modified so it can be used with Event Dump file.
C-                      Use FORMAT statements with lines > 80 characters
C-                      Added info related to FORCE_VERTEX_CENTER.
C-                      Add input bank selection.
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek  
C-                      Print information regarding Forced Andor Terms and
C-                      Trigger Tower Saturation. 
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger   
C-                      Add dump names of andor terms copied from an
C-                      existing TRGR bank found in the input event
C-                      Add switch specifying whether the L0 crate is copied
C-                      Add switch specifying whether the jet lists are created
C-   Updated   4-MAR-1994   Philippe Laurens - MSU L1 Trigger  
C-                      Add switch PRESCALER_USE_EVENT_DATA 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      INTEGER LUN, IER           ! STANDARD OUTPUT UNIT
      CHARACTER*127 BUF
      INTEGER ETA, COUNT
      INTEGER TRULEN
      EXTERNAL TRULEN
C
  100 FORMAT('1', 78('-'))
      WRITE(LUN, 100)
      WRITE(LUN,*)
      WRITE(LUN,*) 'L1SIM RUN PARAMETERS'
      WRITE(LUN,*) '--------------------'
      WRITE(LUN,*)
  200 FORMAT(' ', A, T25, A) 
C
      CALL L1UTIL_EXPAND_FILENAME(
     &            PROGRAMMING_FILE_NAME(1:PROGRAMMING_FILE_NAME_LENGTH),
     &            BUF)
      WRITE(LUN,200) 'Programming File:', BUF(1:TRULEN(BUF))
C
      CALL L1UTIL_EXPAND_FILENAME(
     &            RESOURCE_FILE_NAME(1:RESOURCE_FILE_NAME_LENGTH), 
     &            BUF)
      WRITE(LUN,200) 'Trigger Resource File:', BUF(1:TRULEN(BUF))
C
      CALL L1UTIL_EXPAND_FILENAME(
     &         LOOKUP_TABLE_FILE_NAME(1:LOOKUP_TABLE_FILE_NAME_LENGTH),
     &         BUF)
      WRITE(LUN,200) 'Lookup Table File:', BUF(1:TRULEN(BUF))
      WRITE(LUN,*)
C
      WRITE(LUN,*) 'Input data was taken from ' 
     &  // L1SIM_INPUT_SOURCE // ' banks.'
      WRITE(LUN,*)
C
      WRITE(LUN,*) 'ZEBRA Path: ' // ZEBRA_PATH
C
      IF (DO_PROGRAMMING_LISTING .EQV. .TRUE.) THEN
        WRITE (LUN,*) 
     &    'Trigger programming listing was added to the summary file.'
      ELSE
        WRITE (LUN,*) 
     &'Trigger programming listing was not added to the summary file.'
      ENDIF
C
      IF (DO_FAST_CALTRIG .EQV. .TRUE.) THEN
        WRITE(LUN,*) 
     &  'Fast Calorimeter algorithm was used. (No TRGR bank generated)'
      ELSE
        WRITE(LUN,*) 'Fast Calorimeter algorithm was not used.'
      ENDIF
C
      IF (APPLY_NOISE .EQV. .TRUE.) THEN
        WRITE(LUN,*) 
     &    'Noise was added to Energy From Calorimeter Banks.'
      ELSE
        WRITE(LUN,*) 
     &    'Noise was not added to Energy From Calorimeter Banks.'
      ENDIF
C
      IF (APPLY_PRESCALER .EQV. .TRUE.) THEN
        IF ( PRESCALER_USE_EVENT_DATA .EQV. .TRUE. ) THEN
          WRITE (LUN,*) 'Prescaling mode was enabled,'
          WRITE (LUN,*) ' and used event data in final decision.'
        ELSE
          WRITE (LUN,*) 'Prescaling mode was enabled,'
          WRITE (LUN,*) ' and used programmed ratios (random 1 of n).'
        END IF
      ELSE
        WRITE(LUN,*) 'Prescaling Ratios were ignored.'
      ENDIF
      WRITE(LUN,*)
C
      IF (USE_BLS_GAIN_CORRECTION .EQV. .TRUE.) THEN
        WRITE(LUN,*) 'BLS Gain Correction Coefficients were applied.'
      ELSE
        WRITE(LUN,*) 'BLS Gain Correction Coefficients were ignored.'
      ENDIF
C
      CALL L1UTIL_EXPAND_FILENAME(
     &             BLS_FILE_NAME(1:BLS_FILE_NAME_LENGTH), BUF)
      WRITE(LUN,*) 'BLS Gain Correction File: ' // BUF(1:TRULEN(BUF))
      WRITE(LUN,*)
C
      IF (FORCE_VERTEX_CENTER .EQV. .TRUE.) THEN
        WRITE(LUN,*) 'The Level 0 Vertex was ignored, ' 
     &    // 'the Center Level 1 Lookup Page was always used'
      ELSE
        WRITE(LUN,*) 'The Level 0 Vertex was used '
     &    // 'in computing the Level 1 Lookup Page.'
      ENDIF
      WRITE(LUN,*)
C
  300 FORMAT(' ', A32, 3X, A)
      IF (NUM_FORCED_TERMS .GT. 0) THEN
        WRITE(LUN,*) 'Andor Terms Forced to known state'
        WRITE(LUN,*)
        DO COUNT = 1, NUM_FORCED_TERMS
          IF (FORCED_TERM_STATES(COUNT) .EQV. .TRUE.) THEN
            WRITE(LUN, 300) FORCED_TERM_NAMES(COUNT), 'TRUE'
          ELSE
            WRITE(LUN, 300) FORCED_TERM_NAMES(COUNT), 'FALSE'
          ENDIF
        END DO
      ELSE 
        WRITE(LUN,*) 'No Forced Andor Terms'
      ENDIF
      WRITE(LUN,*)
C
      IF (NUM_COPIED_TERMS .GT. 0) THEN
        WRITE(LUN,*) 'Andor Terms copied from input event'
        WRITE(LUN,*)
        DO COUNT = 1, NUM_COPIED_TERMS
          WRITE(LUN, 400) COPIED_TERM_NAMES(COUNT)
  400     FORMAT('    ', A32 )
        END DO
      ELSE 
        WRITE(LUN,*) 'No Andor Terms copied from input event'
      ENDIF
      WRITE(LUN,*)
C
      WRITE(LUN,*) 'Trigger Tower Saturation Values'
      WRITE(LUN,*)
      WRITE(LUN,*) '|ETA|  EM GeV  HD GeV'
      WRITE(LUN,*) '---------------------'
  500 FORMAT(2X, I2, 3X, F7.2, X, F7.2)
      DO ETA = 1, 20
        WRITE(LUN, 500) ETA, TRIGGER_TOWER_SATURATION(1,ETA),
     &                       TRIGGER_TOWER_SATURATION(2,ETA)
      END DO
      WRITE(LUN,*)
C
      IF (L15_CERTIFIED .EQV. .TRUE.) THEN
        WRITE(LUN,*) 'Level 1.5 decision was certified ' 
     &    // 'on each event (i.e. NOT like hardware)'
      ELSE
        WRITE(LUN,*) 'Level 1.5 decision was calculated as needed'
     &    // ' (i.e. like hardware)'
      ENDIF
      WRITE (LUN,*) 
C
      IF ( COPY_L0_CRATE .EQV. .TRUE.) THEN
        WRITE(LUN,*) 'Level 0 Crate copied from input event to output' 
      ELSE
        WRITE(LUN,*) 'Level 0 Crate not copied to output event' 
      ENDIF
      WRITE (LUN,*) 
C
      IF ( CREATE_JET_LISTS .EQV. .TRUE.) THEN
        WRITE(LUN,*) 'EM and Tot Et Jet Lists written to output event'
      ELSE
        WRITE(LUN,*) 'Jet Lists not written to output event'
      ENDIF
      WRITE (LUN,*) 
C
C----------------------------------------------------------------------
  999 RETURN
      END
