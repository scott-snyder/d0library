      LOGICAL FUNCTION L1SIM_DIALOG()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call user dialog routines for each trigger subsystem.
C-   
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  21-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   9-DEC-1991   Philippe Laurens, Steven Klocek   
C-                      Added call to L15_FRAMEWORK_DIALOG.
C-      
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      L1SIM_DIALOG = .TRUE.
      CALL L1_CALTRIG_DIALOG
      CALL L15_FRAMEWORK_DIALOG
      CALL L1_USER_TERMS_DIALOG
C----------------------------------------------------------------------
  999 RETURN
      END
