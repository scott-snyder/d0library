      LOGICAL FUNCTION PX_CHECK_SYSTEM_COMMAND(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Check the given command to see if it is a system
C-   command.
C-      PX_CHECK_SYSTEM_COMMAND = .TRUE.
C-         If COMMAND = System Command (HARDCOPY,ROTATE,PICK,SUPERIMPOSE,
C-                             ZOOM A VIEW, CHANGE DISPLAY MODE)
C-         Else
C-      PX_CHECK_SYSTEM_COMMAND = .FALSE.
C-
C-   Inputs  : COMMAND [C*]: Command to check
C-   Outputs :
C-   Controls:
C-
C-   Created  10-JUL-1992   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C----------------------------------------------------------------------
      IF ( ( COMMAND .EQ. 'ZOOM A VIEW' ) .OR.
     &     ( COMMAND .EQ. 'HARDCOPY'    ) .OR.
     &     ( COMMAND .EQ. 'PICK'        ) .OR.
     &     ( COMMAND .EQ. 'SUPERIMPOSE' ) .OR.
     &     ( COMMAND .EQ. 'ROTATE'      ) .OR.
     &     ( COMMAND .EQ. 'CHANGE DISPLAY MODE' )   ) THEN
        PX_CHECK_SYSTEM_COMMAND  = .TRUE.
      ELSE
        PX_CHECK_SYSTEM_COMMAND  = .FALSE.
      ENDIF
  999 RETURN
      END
