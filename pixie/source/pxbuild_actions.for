      SUBROUTINE PXBUILD_ACTIONS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Dispatch routine for PXBUILD.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  12-SEP-1990   Harrison B. Prosper
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*40 COMMAND,FILENAME
C----------------------------------------------------------------------
C
C ****  Process commands
C
      COMMAND = ' '
      DO WHILE ( COMMAND .NE. 'EXIT' )
C
        FILENAME = ' '
        CALL MENUDO
     &    ('Build PIXIE User Interface Routine','BUILD',COMMAND)
C
        IF ( COMMAND .NE. 'EXIT' ) THEN
          IF     ( COMMAND .EQ. 'READ' ) THEN
            CALL PXBUILD_READ(FILENAME)
          ELSEIF ( COMMAND .EQ. 'MODIFY RCP FILES' ) THEN
            CALL PXBUILD_MODIFY
          ENDIF
        ENDIF
C
      ENDDO
C
C ****  Clear screen
C
      CALL NORMAL
  999 RETURN
      END
