      LOGICAL FUNCTION TRD_DISPATCH ()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : EXECUTES TRD FUNCTIONS REQUESTED FROM TRD_MENU
C-
C-   Returned value  : TRUE IF OK
C-   Inputs  : NONE
C-   Outputs : NONE
C-   Controls: NONE
C-
C-   Created  13-SEP-1990   J-Fr Glicenstein
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      TRD_DISPATCH = .TRUE.
      CALL EXAMINE_DISPATCH_COMMAND(COMMAND)
      IF (COMMAND .EQ. 'TRD USER DIALOG') THEN
        CALL TRD_USER_DIALOG
      END IF
  999 RETURN
      END
