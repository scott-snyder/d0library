      SUBROUTINE L1DMP_SELECT_SECTIONS(SIMULATION)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Select the sections to include in the dump of the
C-     TRGR bank.
C-
C-   Inputs  : none
C-   Outputs : modified common block L1SIM_CONTROL
C-   Controls: none
C-
C-   Created  15-JAN-1992   Philippe Laurens, Steven Klocek
C-   Updated   9-JUL-1993   Philippe Laurens - MSU L1 Trigger  
C-                          Add Large Tile option 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
C
      LOGICAL SIMULATION
C
      INTEGER I,J
      INTEGER COUNT
      INTEGER NUM_PARS
C
      INTEGER YES, NO
      PARAMETER (YES = 1, NO = 2)
C
      INTEGER SELECTIONS(DMP_NUM_SECTIONS_SIMU)
      INTEGER OPTNUM(DMP_NUM_SECTIONS_SIMU)
      CHARACTER*3 OPTIONS(10, DMP_NUM_SECTIONS_SIMU)
      CHARACTER*42 LABELS(DMP_NUM_SECTIONS_SIMU)
C
      DATA SELECTIONS / YES, YES, YES, NO, NO, NO, NO, NO, NO, NO /
C
      DATA LABELS / 'Control Information', 
     &  'Event Summary Current Event',
     &  'Datablock Contents (PRTRGR)',
     &  'Event Summary Previous Event',
     &  'Andor Term Listing by Name',
     &  'Trigger Tower ADC Values in Counts',
     &  'Trigger Tower ADC Values in GeV',
     &  'Level 1 Datablock Raw Dump',
     &  'Large Tile Energies in GeV',
     &  'Trigger Tower Energy Deposited in GeV' /
C
      DATA ((OPTIONS(I,J), J=1, DMP_NUM_SECTIONS_SIMU), I = 1, 2) 
     &  / DMP_NUM_SECTIONS_SIMU*'Yes', DMP_NUM_SECTIONS_SIMU*'No' /
C
      DATA OPTNUM / DMP_NUM_SECTIONS_SIMU*2 /
C
      IF (SIMULATION .EQV. .TRUE.) THEN
        NUM_PARS = DMP_NUM_SECTIONS_SIMU
      ELSE
        NUM_PARS = DMP_NUM_SECTIONS_TRGR
      ENDIF
C
      CALL GETOPT(NUM_PARS, LABELS, OPTNUM, OPTIONS, SELECTIONS)
C
C       Transfer the selections to the dump routine.
C
      CALL L1_FW_AND_CT_DUMP_SELECT
     &  (DMP_SIMULATION_PERFORMED, SIMULATION)
C
      DO COUNT = DMP_CONTROL, DMP_NUM_SECTIONS_SIMU
        IF (SELECTIONS(COUNT) .EQ. NO) THEN
          CALL L1_FW_AND_CT_DUMP_SELECT(COUNT, .FALSE.)
        ELSE
          CALL L1_FW_AND_CT_DUMP_SELECT(COUNT, .TRUE.)
        ENDIF
      END DO
C----------------------------------------------------------------------
  999 RETURN
      END
