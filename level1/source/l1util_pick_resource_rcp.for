      FUNCTION L1UTIL_PICK_RESOURCE_RCP()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call EZPICK with the correct bank name to retrieve
C-      Andor Term numbers.
C-
C-   Returned value  : Success if .TRUE.
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   6-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
C
      LOGICAL L1UTIL_PICK_RESOURCE_RCP
C
      INTEGER IER
      CHARACTER*72 STRING
C
      CALL EZPICK(ANDOR_RESOURCE_RCPKEY)
      CALL EZERR(IER)
C
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG(' EZPICK','L1UTIL_PICK_RESOURCE_RCP',STRING,'F')
        L1UTIL_PICK_RESOURCE_RCP = .FALSE.
      ELSE
        L1UTIL_PICK_RESOURCE_RCP = .TRUE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
