      FUNCTION L1UTIL_PICK_L15RESOURCE_RCP ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Call EZPICK to select the correct RCP bank for
C-      Level 1.5 Term Resources. It should later be released with a
C-      call to EZRSET.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created   4-DEC-1991   Philippe Laurens, Steven Klocek   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
C
      LOGICAL L1UTIL_PICK_L15RESOURCE_RCP
C
      INTEGER IER
      CHARACTER*80 STRING
C
      CALL EZPICK (L15_TERM_RESOURCE_RCP)
      CALL EZERR (IER) 
C
      IF (IER .NE. 0) THEN
        CALL EZGET_ERROR_TEXT(IER, STRING)
        CALL ERRMSG(' EZPICK','L1UTIL_PICK_L15RESOURCE_RCP',STRING,'F')
        L1UTIL_PICK_L15RESOURCE_RCP = .FALSE.
      ELSE
        L1UTIL_PICK_L15RESOURCE_RCP = .TRUE.
      ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
