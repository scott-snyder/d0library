      SUBROUTINE PX_NEW_DISPLAYS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Show a menu for executing or creating new
C-   multi-view displays, interactively.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  15-NOV-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL MENUDO('New Displays','NEWDISP',COMMAND)
        CALL INTMSG(' Sorry, not yet implemented')
        IF     ( COMMAND .EQ. 'EXECUTE DISPLAY' )  THEN
        ELSEIF ( COMMAND .EQ. 'CREATE DISPLAY' )THEN
        ELSEIF ( COMMAND .EQ. 'SAVE DISPLAY' )  THEN
        ELSEIF ( COMMAND .EQ. 'LIST' )  THEN
        ENDIF
      ENDDO
  999 RETURN
      END
