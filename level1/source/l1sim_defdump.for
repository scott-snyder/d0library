      LOGICAL FUNCTION L1SIM_DEFDUMP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call an event dump defining routine for each trigger
C-    subsystem.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      L1SIM_DEFDUMP = .TRUE.
      CALL L1_FW_AND_CT_DEFDUMP
      CALL L1_USER_TERMS_DEFDUMP
C----------------------------------------------------------------------
  999 RETURN
      END
