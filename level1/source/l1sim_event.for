      FUNCTION L1SIM_EVENT()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call an event processing routine for each trigger 
C-      subsystem.
C-
C-   Returned value  : .FALSE. if no triggers fired, .TRUE. otherwise
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                            - Changed function declaration to pass D0FLAVOR.
C-   Updated   7-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Added call to L1_SPECIAL_TERMS_SIM, to simulate
C-                      special-purpose Andor Terms.
C-   Updated   3-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Added support for actual Level 1.5 Subsystems. 
C-   Updated   9-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Add Level 1.5 muon trigger
C-                      Cause analysis routine to be called on all events. 
C-                      Add call to L15_FRAMEWORK_ANAL
C-   Updated  13-DEC-1991   Philippe Laurens, Steven Klocek  
C-                      Replace calls to L1_FW_AND_CT_ANAL and 
C-                        L15_FRAMEWORK_ANAL with calls to L1_AND_L15_FW_ANAL
C-                        and L1_CALTRIG_ANAL.
C-   Updated   6-FEB-1992   Philippe Laurens, Steven Klocek  
C-                      Fill the internal copy of the datablock after the Level
C-                      1 Framework has been simulated.  Always do this, even
C-                      if no triggers have fired. 
C-   Updated  14-FEB-1992   Philippe Laurens, Steven Klocek  
C-                      Add the Data Cable Trailer to the TRGR bank after all
C-                      subsystem *_FILL routines have been called.
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek  
C-                      Compute the proper number of Specific Triggers that
C-                      passed Level 1 and Level 1.5
C-   Updated   23-MAR-1992  Amber Boehnlein, added call to ESUM filling routine
C-   Updated  14-JUL-1992   Philippe Laurens, Steven Klocek   
C-                      - pass argument to L1_CALTRIG_TOWERS and skip event 
C-                         if input data is missing
C-                      - remove ESUM filling routines (now done in Level 2)
C-   Updated  12-AUG-1992   Philippe Laurens, Steven Klocek  
C-                      Reorganize code so that:
C-                      1) The Level 1.5 information is put in Data Block
C-                         after Level 1.5 simulation. The Level 1.5 section of
C-                         the Data Block is untouched if a Level 1.5 cycle 
C-                         was not required for an event.
C-   Updated  25-NOV-1992   Philippe Laurens, Steven Klocek  
C-                      Assignment of L15_CYCLE_PERFORMED moved here from
C-                      L1_LEVEL15_NEEDED. 
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add copy L0 Crate from input event 
C-   Updated  28-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Add Large Tile  
C-   Updated  25-MAR-1994   sFahey  Added L15 Calorimeter sim
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
C
      LOGICAL L1SIM_EVENT
      LOGICAL L1_LEVEL15_NEEDED
C
      INTEGER  GZTRGR
      EXTERNAL GZTRGR
C
      INTEGER IER
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
      INTEGER UNPACK_ANDOR_ERROR
      CHARACTER*80 MESSAGE
      INTEGER SLEN
C
      LOGICAL L15_TERM_STATES(1:NUM_L15_TERM)
      INTEGER L15_TERM_INDICES(1:NUM_L15_TERM)
      INTEGER NUM_L15_TERM_USED
      LOGICAL ERROR
      LOGICAL INPUT_FOUND
C
      INTEGER LTRGR
      INTEGER LSUBSYSTEM
      INTEGER NUM_TRIGGERS_FIRED
      LOGICAL USING_FAST
C
C       ==================================
C       
      L1SIM_EVENT = .TRUE.
C
C       Clear the Andor Term states
C       ===========================
C
      CALL L1FW_CLEAR_ANDOR
      CALL L15FW_CLEAR_TERMS
C
C       Unpack energies into Trigger Towers
C       ===================================
C       
      CALL L1_CALTRIG_TOWERS ( INPUT_FOUND )
      IF ( INPUT_FOUND .EQV. .FALSE. ) THEN
        L1SIM_EVENT = .FALSE.
        GOTO 999 
      ENDIF
C
C       Simulate Level 1 trigger subsystems 
C       ===================================
C       
C       Level 0
C       -------
      CALL      L1_LEVEL0_SIM     ( NUM_ANDOR_USED, 
     &                              ANDOR_STATES, 
     &                              ANDOR_INDICES)
      CALL      L1FW_UNPACK_ANDOR ( NUM_ANDOR_USED, 
     &                              ANDOR_STATES, 
     &                              ANDOR_INDICES, 
     &                              UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' LEVEL0 ANDOR TERMS',
     &                    'L1SIM_EVENT', 
     &                    'Level 0 Andor Terms: ' // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Level 1 Calorimeter trigger
C       ---------------------------
      CALL      L1_CALTRIG_SIM    ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES )
      CALL      L1FW_UNPACK_ANDOR ( NUM_ANDOR_USED, 
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES, 
     &                              UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' CALTRIG ANDOR INDICES',
     &                    'L1SIM_EVENT', 
     &                    'Calorimeter Trigger Andor Terms: ' 
     &                    // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Large Tile Extension to Cal Trig 
C       --------------------------------
      CALL      L1_LARGE_TILE_SIM ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES )
      CALL      L1FW_UNPACK_ANDOR ( NUM_ANDOR_USED, 
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES, 
     &                              UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' LRG TILE ANDOR INDICES',
     &                    'L1SIM_EVENT', 
     &                    'Large Tile Andor Terms: ' 
     &                    // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Level 1 MUON Trigger 
C       --------------------
      CALL      L1_MUON_SIM       ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES )
      CALL      L1FW_UNPACK_ANDOR ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES, 
     &                              UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' MUON ANDOR INDICES',
     &                    'L1SIM_EVENT', 
     &                    'Muon Trigger Andor Terms: ' 
     &                    // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Special purpose Level 1 Andor Terms
C       -----------------------------------
      CALL      L1_SPECIAL_TERMS_SIM ( NUM_ANDOR_USED, 
     &                                 ANDOR_STATES,
     &                                 ANDOR_INDICES )
      CALL      L1FW_UNPACK_ANDOR    ( NUM_ANDOR_USED,
     &                                 ANDOR_STATES,
     &                                 ANDOR_INDICES,
     &                                 UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' SPECIAL ANDOR INDICES',
     &                    'L1SIM_EVENT', 
     &                    'Special Terms Andor Terms: ' 
     &                    // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Optional Level 1 user provided subsystem
C       -----------------------------------------
      CALL      L1_USER_TERMS_SIM ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES )
      CALL      L1FW_UNPACK_ANDOR ( NUM_ANDOR_USED,
     &                              ANDOR_STATES,
     &                              ANDOR_INDICES, 
     &                              UNPACK_ANDOR_ERROR )
      IF ( UNPACK_ANDOR_ERROR .NE. UNPACK_ANDOR_OK ) THEN
        CALL    L1FW_UNPACK_ANDOR_ERROR_STRING ( UNPACK_ANDOR_ERROR, 
     &                                           MESSAGE, 
     &                                           SLEN )
        CALL    ERRMSG  ( ' USER TERMS ANDOR INDICES',
     &                    'L1SIM_EVENT', 
     &                    'User Terms Andor Terms: ' 
     &                    // MESSAGE(1:SLEN), 
     &                    'F' )
      ENDIF
C
C       Calculate Specific Trigger states after Level 1
C       ===============================================
C
      CALL L1_FRAMEWORK_SIM
C
C       Fill the internal copy of the Level 1 Datablock
C       ===============================================
C       for every event, to maintain current/previous.
C
      CALL L1DBB_DATA_BLOCK_BUILDER
      CALL L15CALDBB_DATA_BLOCK_BUILDER
C
C       Handle events requiring Level 1.5
C       =================================
C
      L15_CYCLE_PERFORMED = L1_LEVEL15_NEEDED()
      IF ( L15_CYCLE_PERFORMED .EQV. .TRUE. ) THEN
C
C       Level 1.5 MUON trigger
C       ----------------------
        CALL    L15_MUON_SIM       ( NUM_L15_TERM_USED, 
     &                               L15_TERM_STATES,
     &                               L15_TERM_INDICES)
        CALL    L15FW_UNPACK_TERMS ( NUM_L15_TERM_USED, 
     &                               L15_TERM_STATES, 
     &                               L15_TERM_INDICES, 
     &                               ERROR)
        IF ( ERROR .EQV. .TRUE. ) THEN
          CALL  ERRMSG   ( ' MUON TERMS 1.5 INDICES', 
     &                     'L1SIM_EVENT', 
     &                     'Muon Simulator Level 1.5 Terms Conflict', 
     &                     'F')
        ENDIF
C
C       Level 1.5 Calorimeter Trigger
C       -----------------------------
        CALL    L15_CAL_SIM        ( NUM_L15_TERM_USED,
     &                               L15_TERM_STATES,
     &                               L15_TERM_INDICES)
        CALL    L15FW_UNPACK_TERMS ( NUM_L15_TERM_USED,
     &                               L15_TERM_STATES,
     &                               L15_TERM_INDICES,
     &                               ERROR)
        IF ( ERROR .EQV. .TRUE. ) THEN
          CALL  ERRMSG   ( ' CAL TERMS 1.5 INDICES',
     &                     'L1SIM_EVENT',
     &                     'Cal Simulator Level 1.5 Terms Conflict',
     &                     'F')
        ENDIF

C
C       Optional level 1.5 user provided subsystem
C       ------------------------------------------
        CALL    L15_USER_TERMS_SIM ( NUM_L15_TERM_USED, 
     &                               L15_TERM_STATES,
     &                               L15_TERM_INDICES)
        CALL    L15FW_UNPACK_TERMS ( NUM_L15_TERM_USED, 
     &                               L15_TERM_STATES, 
     &                               L15_TERM_INDICES, 
     &                               ERROR)
        IF ( ERROR .EQV. .TRUE. ) THEN
          CALL  ERRMSG  ( ' USER TERMS 1.5 INDICES', 
     &                    'L1SIM_EVENT', 
     &                    'User Terms Level 1.5 Terms Conflict', 
     &                    'F' )
        ENDIF
C
C       Calculate Specific Trigger states after Level 1.5 processing
C       ============================================================
C
        CALL L15_FRAMEWORK_SIM
C
      ENDIF
C
C       The Level 1.5 section of the data block is not updated in hardware
C       except for events where a Level 1.5 cycle is performed. On other
C       events, the Level 1.5 section reflects the states as of the last event
C       where a Level 1.5 cycle was performed. Certain Level 1.5 quantities
C       elsewhere in the Data Block are updated on each event, such as the 
C       Level 1.5 Potential Event scaler and the flag indicating whether the 
C       Level 1.5 system was used.
C
      CALL L1DBB_L15_SECTION(L15_CYCLE_PERFORMED)
C       
C       Count the survivors
C       ===================
C       all these counts are kept in L1_SPECIFIC_TRIGGER.INC         
C       NUM_SPTRG_PASS_L1 is initialized and computed in L1_FRAMEWORK_SIM
C       NUM_L15_SPTRG_SENT_L15 is initialized and computed in L1_LEVEL15_NEEDED
C       NUM_L15_SPTRG_PASS_L15 is initialized in L1_LEVEL15_NEEDED
C                              and computed in L15_FRAMEWORK_SIM
C
      NUM_SPTRG_PASS_L1SIM = NUM_SPTRG_PASS_L1 - 
     &        ( NUM_L15_SPTRG_SENT_L15 - NUM_L15_SPTRG_PASS_L15 )
C
C       Perform analysis and record histogram values. 
C
      CALL L1_AND_L15_FW_ANAL
      CALL L1_CALTRIG_ANAL
C
C       Banks are filled only if at least one trigger has fired for this event
C       ======================================================================
C
      IF ( NUM_SPTRG_PASS_L1SIM .EQ. 0 ) THEN
        L1SIM_EVENT = .FALSE.
        GOTO 100 
      ENDIF
C
C       Fast Simulation does not fill any banks.
C       =======================================
C
      CALL L1C_USING_FAST_SIMULATION(USING_FAST)
      IF (USING_FAST .EQV. .TRUE.) THEN
        L1SIM_EVENT = .FALSE.
        GOTO 100 
      ENDIF
C
C       Fill TRGR bank (and any others) which subsystems may need.
C       =========================================================
C
      CALL L1_FW_AND_CT_FILL
      CALL L1_MUON_FILL
      CALL L1_L15CAL_FILL
      CALL L1_USER_TERMS_FILL
      CALL L1_COPY_CRATES
C
C       Add the Data Cable Trailer to the end of the TRGR bank.
C       =======================================================
C
      LTRGR = GZTRGR()
      CALL L1UTIL_TRGR_EXTENSION(LTRGR, 16, LSUBSYSTEM) ! Add 16 words 
C
  100 CONTINUE
C
C----------------------------------------------------------------------
  999 CONTINUE
      RETURN
      END
