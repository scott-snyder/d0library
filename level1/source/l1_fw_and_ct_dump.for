      SUBROUTINE L1_FW_AND_CT_DUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dump Level1 Trigger bank
C-
C-
C-   ENTRY L1_FW_AND_CT_DUMP_ENABLE(PERFORM_DUMP)
C-            Enable/disable dump.
C-
C-   Created   2-Oct-1990    Maris A. Abolins
C-   Updated  Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                        - Now performs more dumps, and calls routines to
C-                          verify the fast simulation, if that verification is
C-                          selected. 
C-                        - Changed from a function to a subroutine.
C-                        - Changed name of routine from L1C_DMP to
C-                          L1_FW_AND_CT_DUMP. 
C-                        - Changed name of entry point from L1C_DEFD to
C-                          L1_FW_AND_CT_DEFDUMP. 
C-                        - Replaced D0$INC:TRG_SIMUL_RAW_EVENT.INC with
C-                          D0$INC:L1C_EVENT_RAW.INC 
C-   Updated   8-NOV-1991   Philippe Laurens, Steven Klocek   
C-                      Bypass PRTRGR, directly call 
C-                        L1DMP_DBLOCK_UNPACK and L1DMP_TOWER_UNPACK
C-   Updated  17-JAN-1992   Philippe Laurens, Steven Klocek   
C-                      Support for expanded dump capability.
C-                      Moved entry point L1_FW_AND_CT_DEFDUMP to seperate
C-                        routine.
C-                      Added entry point L1_FW_AND_CT_DUMP_ENABLE which simply
C-                        enables/disables using a logical argument.
C-                      Added entry pont L1_FW_AND_CT_DUMP_SELECT which is used
C-                        to select/deselect sections of the dump output.
C-   Updated  24-FEB-1992   Philippe Laurens, Steven Klocek   
C-                              use GZFIND_CRATE to find actual data
C-   Updated   9-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                          Add Large Tile option 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1DBB_DATA_BLOCK.PARAMS'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
C
      LOGICAL YES
C
      INTEGER LUN                       ! UNIT NUMBER FOR USER UNIT
      INTEGER DMPUNI
      INTEGER GZTRGR, LTRGR, GZFIND_CRATE, LTRGR_LEVEL1
      EXTERNAL DMPUNI, GZTRGR, GZFIND_CRATE
      CHARACTER*72 VLEVEL1
      EXTERNAL VLEVEL1
      LOGICAL PASSED_VERIFY
C
      LOGICAL CURRENT, PREVIOUS
      PARAMETER ( CURRENT = .TRUE., PREVIOUS = .FALSE. )
C
      INTEGER SECTION
      LOGICAL INCLUDE
      LOGICAL PERFORM_DUMP
C
      LOGICAL PR_SIMULATION_PERFORMED
      LOGICAL PR_CONTROL
      LOGICAL PR_EVENT_CURR
      LOGICAL PR_DATABLOCK
      LOGICAL PR_EVENT_PREV
      LOGICAL PR_ANDOR_TERMS
      LOGICAL PR_ADC_COUNTS
      LOGICAL PR_ADC_GEV
      LOGICAL PR_DATABLOCK_RAW
      LOGICAL PR_TOWER_ENERGY
      LOGICAL PR_LGTILE_ENERGY
C
      SAVE PR_SIMULATION_PERFORMED, PR_CONTROL, PR_EVENT_CURR,
     &  PR_DATABLOCK,
     &  PR_EVENT_PREV, PR_ANDOR_TERMS, PR_ADC_COUNTS, 
     &  PR_ADC_GEV, PR_DATABLOCK_RAW, PR_TOWER_ENERGY, PR_LGTILE_ENERGY
C
      SAVE YES 
      DATA YES/.TRUE./
C
      DATA PR_SIMULATION_PERFORMED / .FALSE. /
      DATA PR_CONTROL              / .TRUE. /
      DATA PR_EVENT_CURR           / .TRUE. /
      DATA PR_DATABLOCK            / .TRUE. /
      DATA PR_EVENT_PREV           / .FALSE. /
      DATA PR_ANDOR_TERMS          / .FALSE. /
      DATA PR_ADC_COUNTS           / .TRUE. /
      DATA PR_ADC_GEV              / .FALSE. /
      DATA PR_DATABLOCK_RAW        / .FALSE. /
      DATA PR_TOWER_ENERGY         / .FALSE. /
      DATA PR_LGTILE_ENERGY        / .FALSE. /
C----------------------------------------------------------------------
C
      IF (YES) THEN
C
        LTRGR = GZTRGR ( )
        LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', LTRGR, CRATE_ID ) 
        IF ( LTRGR_LEVEL1 .LE. 0 ) THEN 
          CALL ERRMSG( 'No Level 1 Crate', 'L1_FW_AND_CT_DUMP', 
     &      ' Couldnt find LEVEL 1 data ', 'W')
          GOTO 999        
        END IF 
C        
        LUN = DMPUNI()
        IF (PR_SIMULATION_PERFORMED .EQV. .FALSE.) THEN
          CALL L1UTIL_TRGR_UNPACK( LTRGR_LEVEL1, CURRENT )
        ENDIF
C
C       Write the table of contents
C
        WRITE(LUN,*) 
        WRITE(LUN,*)
        WRITE(LUN,*) 'Information Included in This Dump'
        WRITE(LUN,*) '================================='
        WRITE(LUN,*)
  190   FORMAT(' ', 65('.'), T2, A, T65, A)
C
        IF (PR_CONTROL .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Control Information', 'YES'
        ELSE
          WRITE(LUN,190) 'Control Information', 'NO'
        ENDIF
C
        IF (PR_EVENT_CURR .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Event Summary (Current Event)', 'YES'
        ELSE
          WRITE(LUN,190) 'Event Summary (Current Event)', 'NO'
        ENDIF
C
        IF (PR_DATABLOCK .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Level 1 Datablock Contents (PRTRGR)', 'YES'
        ELSE
          WRITE(LUN,190) 'Level 1 Datablock Contents (PRTRGR)', 'NO'
        ENDIF
C
        IF (PR_EVENT_PREV .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Event Summary (Previous Event)', 'YES'
        ELSE
          WRITE(LUN,190) 'Event Summary (Previous Event)', 'NO'
        ENDIF
C
        IF (PR_ANDOR_TERMS .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Andor Term Listing by Name', 'YES'
        ELSE
          WRITE(LUN,190) 'Andor Term Listing by Name', 'NO'
        ENDIF
C
        IF (PR_ADC_COUNTS .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Trigger Tower ADC Values in Counts', 'YES'
        ELSE
          WRITE(LUN,190) 'Trigger Tower ADC Values in Counts', 'NO'
        ENDIF
C
        IF (PR_ADC_GEV .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Trigger Tower ADC Values in GeV', 'YES'
        ELSE
          WRITE(LUN,190) 'Trigger Tower ADC Values in GeV', 'NO'
        ENDIF
C
        IF (PR_SIMULATION_PERFORMED .EQV. .TRUE.) THEN
          IF (PR_LGTILE_ENERGY .EQV. .TRUE.) THEN
            WRITE(LUN,190) 
     &  'Dump Of Large Tile Energies (In GeV)',
     &        'YES'
          ELSE
            WRITE(LUN,190) 
     &  'Dump Of Large Tile Energies (In GeV)',
     &        'NO'
          ENDIF
        ENDIF
C
        IF (PR_SIMULATION_PERFORMED .EQV. .TRUE.) THEN
          IF (PR_TOWER_ENERGY .EQV. .TRUE.) THEN
            WRITE(LUN,190) 
     &  'Dump Of Trigger Tower Energy From Calorimeter Banks (In GeV)',
     &        'YES'
          ELSE
            WRITE(LUN,190) 
     &  'Dump Of Trigger Tower Energy From Calorimeter Banks (In GeV)',
     &        'NO'
          ENDIF
        ENDIF
C
        IF (PR_DATABLOCK_RAW .EQV. .TRUE.) THEN
          WRITE(LUN,190) 'Raw Datablock Dump', 'YES'
        ELSE
          WRITE(LUN,190) 'Raw Datablock Dump', 'NO'
        ENDIF
C
C       
C
  200   FORMAT(' ', A, A)
        IF (PR_CONTROL .EQV. .TRUE.) THEN
          WRITE(LUN,*)
          WRITE(LUN,*)
          WRITE(LUN,*) 'Control Information'
          WRITE(LUN,*) '==================='
          WRITE(LUN,*)
          WRITE(LUN,200) 'Version: ', VLEVEL1()
          WRITE(LUN,*)
          IF (PR_SIMULATION_PERFORMED .EQV. .TRUE.) THEN
            WRITE(LUN,*) 'TRGR bank generated by simulation.'
            WRITE(LUN,*)
            CALL L1_AND_L15_CONTROL_SSUM(LUN)
          ELSE
            WRITE(LUN,*) 'Dump of existing TRGR bank.'
          ENDIF
        ENDIF
C
        IF (PR_EVENT_CURR .EQV. .TRUE.) THEN
          WRITE (LUN,*)
          WRITE (LUN,*)
          WRITE (LUN,*) 'Event Summary (Current Event)'
          WRITE (LUN,*) '============================='
          CALL L1DMP_DBLOCK_UNPACK( LUN, PR_SIMULATION_PERFORMED)
        ENDIF
C
        IF (PR_DATABLOCK .EQV. .TRUE.) THEN
          WRITE(LUN,*)
          WRITE(LUN,*)
          CALL PRTRGR(LUN, LTRGR, 0, 'ONE', 1)
        ENDIF
C
        IF (PR_EVENT_PREV .EQV. .TRUE.) THEN
          WRITE (LUN,*)
          WRITE (LUN,*)
          WRITE (LUN,*) 'Event Summary (Previous Event)' 
          WRITE (LUN,*) '=============================='
          CALL L1UTIL_TRGR_UNPACK( LTRGR_LEVEL1, PREVIOUS )
          CALL L1DMP_DBLOCK_UNPACK(LUN, PR_SIMULATION_PERFORMED)
          CALL L1UTIL_TRGR_UNPACK( LTRGR_LEVEL1, CURRENT ) 
        ENDIF
C
        IF (PR_ANDOR_TERMS .EQV. .TRUE.) THEN
          CALL L1DMP_ANDOR_TERMS(LUN, PR_SIMULATION_PERFORMED)
        ENDIF
C
        IF (PR_ADC_COUNTS .EQV. .TRUE.) THEN
          WRITE(LUN,*)
          WRITE(LUN,*)
          CALL PRTRGR(LUN, LTRGR, 0, 'ONE', 2)
        ENDIF
C
        IF (PR_ADC_GEV .EQV. .TRUE.) THEN
          CALL L1DMP_ADC_GEV_UNPACK(LUN)
        ENDIF
C
        IF (PR_TOWER_ENERGY .EQV. .TRUE.) THEN
          CALL L1DMP_TOWER_UNPACK(LUN)
        ENDIF
C
        IF (PR_LGTILE_ENERGY .EQV. .TRUE.) THEN
          CALL L1DMP_LARGE_TILE_GEV(LUN)
        ENDIF
C
        IF (PR_DATABLOCK_RAW .EQV. .TRUE.) THEN
          CALL L1DMP_DBLOCK_RAW(LUN, 'HL') 
        ENDIF
C
C     If the debugging dumps compilation flag is set, dump the Fast Simulation
C       results
C
        IF (LV1_DEBUGGING_DUMPS .EQV. .TRUE.) THEN
          CALL L1DMP_FASTSIMUL( LUN )
        ENDIF
C
C     If the verify fast simulation compilation flag is set, compare the
C       results of the fast Calorimeter Trigger algorithm with the normal
C       Calorimeter Trigger algorithm. Do this only if simulation was performed
C       on this event.
C
        IF ((VERIFY_FAST_SIMULATION .EQV. .TRUE.) 
     &    .AND. (PR_SIMULATION_PERFORMED .EQV. .TRUE.)) THEN
          CALL L1C_COMPARE_CALTRIG_RSLT(PASSED_VERIFY)
          IF (PASSED_VERIFY .EQV. .FALSE.) THEN
            CALL ERRMSG (' L1C: Fast simulation bad',
     &        'L1_FW_AND_CT_DUMP',
     &        ' The fast simulation results did not match' //
     &        ' the normal simulation results.','W')
            CALL L1C_COMPARE_CALTRIG_MESSAGE(LUN, LV1_DUMP_DATABLOCK)
            CALL L1DMP_FASTSIMUL( LUN )
            CALL L1C_SWAP_CALTRIG_RSLT()
            CALL L1C_COMPARE_CALTRIG_MESSAGE(LUN, LV1_DUMP_FASTSIMUL)
            CALL L1DMP_FASTSIMUL( LUN )
            CALL L1C_SWAP_CALTRIG_RSLT()
          ENDIF
        ENDIF
      ENDIF
C
  999 CONTINUE
      RETURN
C
C
C
      ENTRY L1_FW_AND_CT_DUMP_ENABLE(PERFORM_DUMP)
C
      YES = PERFORM_DUMP
C
      RETURN
C
C
C
C
C
C
      ENTRY L1_FW_AND_CT_DUMP_SELECT(SECTION, INCLUDE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select the sections to include in the Level 1 
C-     FW and CT dump.
C-
C-   Inputs  : SECTION  The parameter from D0$PARAMS:L1SIM_CONTROL.PARAMS
C-                      selecting a section of the dump.
C-             INCLUDE  A logical indicating whether to include the selected
C-                      section.
C-   Outputs : none
C-   Controls: none
C-
C-   Created  20-JAN-1992   Philippe Laurens, Steven Klocek
C-   Updated   9-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                          Add Large Tile option 
C-
C----------------------------------------------------------------------
      IF (SECTION .EQ. DMP_SIMULATION_PERFORMED) THEN
        PR_SIMULATION_PERFORMED = INCLUDE
      ELSEIF (SECTION .EQ. DMP_CONTROL) THEN
        PR_CONTROL = INCLUDE
      ELSEIF (SECTION .EQ. DMP_EVENT_CURR) THEN
        PR_EVENT_CURR = INCLUDE
      ELSEIF (SECTION .EQ. DMP_DATABLOCK) THEN
        PR_DATABLOCK = INCLUDE
      ELSEIF (SECTION .EQ. DMP_EVENT_PREV) THEN
        PR_EVENT_PREV = INCLUDE
      ELSEIF (SECTION .EQ. DMP_ANDOR_TERMS) THEN
        PR_ANDOR_TERMS = INCLUDE
      ELSEIF (SECTION .EQ. DMP_ADC_COUNTS) THEN
        PR_ADC_COUNTS = INCLUDE
      ELSEIF (SECTION .EQ. DMP_ADC_GEV) THEN
        PR_ADC_GEV = INCLUDE
      ELSEIF (SECTION .EQ. DMP_DATABLOCK_RAW) THEN
        PR_DATABLOCK_RAW = INCLUDE
      ELSEIF (SECTION .EQ. DMP_TOWER_ENERGY) THEN
        PR_TOWER_ENERGY = INCLUDE
      ELSEIF (SECTION .EQ. DMP_LGTILE_ENERGY) THEN
        PR_LGTILE_ENERGY = INCLUDE
      ENDIF
C
      RETURN
      END
