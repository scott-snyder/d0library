      LOGICAL FUNCTION L1SIM_SSUM()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call a standard summary routine for each trigger
C-    subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                            - Changed function declaration to pass D0FLAVOR.
C-   Updated  12-DEC-1991   Philippe Laurens, Steven Klocek
C-                            - Added call to L1_AND_L15_RUN_STATS which adds
C-                              trigger firing statistics to the summary file.
C-                            - Added call to L1_AND_L15_CONTROL_SSUM which
C-                              prints the values of several variables in the
C-                              L1SIM_CONTROL common.
C-   Updated  30-JAN-1992   Philippe Laurens, Steven Klocek
C-                      Modified so can use control sssum with event dumps.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER SSUNIT
      EXTERNAL SSUNIT
C
      L1SIM_SSUM = .TRUE.
      CALL L1_AND_L15_CONTROL_SSUM(SSUNIT())
      CALL L1_AND_L15_RUN_STATS
      CALL L1_FW_AND_CT_SSUM
      CALL L1_USER_TERMS_SSUM
C----------------------------------------------------------------------
  999 RETURN
      END
