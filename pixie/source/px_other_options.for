      SUBROUTINE PX_OTHER_OPTIONS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Show a menu containing other PIXIE system
C-   options.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls: None
C-
C-   Created  20-FEB-1991   Harrison B. Prosper   
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 COMMAND
C----------------------------------------------------------------------
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
        CALL MENUDO('Other Options','PXOPTION',COMMAND)
        IF     ( COMMAND .EQ. 'START REPEAT DISPLAY' ) THEN
          CALL RUNCOM
        ELSEIF ( COMMAND .EQ. 'END REPEAT DISPLAY'   ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ELSEIF ( COMMAND .EQ. 'START LOGIN FILE'     ) THEN
          CALL RUNLOG
        ELSEIF ( COMMAND .EQ. 'STOP LOGIN FILE'      ) THEN
          CALL ENDLOG
        ELSEIF ( COMMAND .EQ. 'LIST DISPLAYS'        ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ELSEIF ( COMMAND .EQ. 'EXECUTE DISPLAY'      ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ELSEIF ( COMMAND .EQ. 'CREATE DISPLAY'       ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ELSEIF ( COMMAND .EQ. 'SAVE DISPLAY'         ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ELSEIF ( COMMAND .EQ. 'PAUSE'                ) THEN
          CALL INTMSG(' Sorry, not yet implemented')
        ENDIF
      ENDDO
  999 RETURN
      END
