      SUBROUTINE L1UTIL_PICK_L1SIM_RCP
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call EZPICK to select the correct RCP bank for
C-      global L1SIM run time parameters. It should later be released with 
C-      call to EZRSET.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  12-AUG-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-                          - Changed to get RCP file name from
C-                            L1SIM_CONTROL.PARAMS 
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
C
      INTEGER IERR
      CHARACTER*80 STRING
C
      CALL EZPICK (L1SIM_RCPFILE)
      CALL EZERR (IERR)               ! CHECK IF ERROR
      IF (IERR.NE.0) THEN
        CALL EZGET_ERROR_TEXT(IERR,STRING)
        CALL ERRMSG (' EZPICK ERR','EZPICK',STRING,'F')
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
