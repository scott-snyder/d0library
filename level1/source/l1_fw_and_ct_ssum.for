      SUBROUTINE L1_FW_AND_CT_SSUM ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Adds Lookup Table Print and Trigger Config.
C-                         to standard summary file.
C-
C-   Returned value  : .TRUE. if ok .FALSE. otherwise
C-   Inputs  : 
C-   Outputs : 
C-   Controls: 
C-
C-   Created   2-OCT-1990   Maris Abolins
C-   Updated  18-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Removed references to LOOKUP_TABLE_PRINT 
C-                            Changed from function to a subroutine.
C-                          - Changed to use L1SIM_CONTROL.INC to communicate
C-                            with other routines rather than entry points
C-                          - Changed name of routine from L1C_SSUM to
C-                            L1_FW_AND_CT_SSUM. 
C-                          - Changed all occurances of
C-                            SP_TRIG_DEFINITION_PRINT to L1DMP_SP_TRIG_DEF. 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      INTEGER LUN ,SSUNIT               ! STANDARD OUTPUT UNIT
C
C----------------------------------------------------------------------
      LUN = SSUNIT ()
C
      IF (DO_PROGRAMMING_LISTING) CALL L1DMP_SP_TRIG_DEF (LUN)
C       
      RETURN
      END
