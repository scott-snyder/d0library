      SUBROUTINE PU_GET_SCREEN_INDEX(SCREEN_COMMAND,IDX,IER)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Given a screen name return the index,
C-   that is, the pointer within the current PXSCREEN array,
C-   corresponding to start of the parameters for that screen.
C-
C-   Inputs  : SCREEN_COMMAND   [C*]: Screen command
C-
C-   Outputs : IDX        [I ]: Index into screen array for given command
C-             IER        [I ]: Error flag.
C-                              If IER = 2 the requested screen was not
C-                              found.
C-   Controls: None
C-
C-   Created  17-OCT-1990   Lupe Howell and Harrison B. Prosper
C-   Updated   5-DEC-1990   Harrison B. Prosper
C-      Can now use package name
C-   Updated  14-JAN-1991   Harrison B. Prosper
C-      Remove array name argument; not needed anymore
C-   Updated   6-MAY-1991   Harrison B. Prosper
C-      Tidy up a little
C-   Updated  15-MAY-1991   Harrison B. Prosper  
C-      Changed argument in PU_GOTO_SCREEN 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) SCREEN_COMMAND
      INTEGER IDX,IER
C
      INTEGER ISCREEN,NSCREEN
C----------------------------------------------------------------------
C
C ****  Get the index for the given screen name
C
      CALL PU_GET_SCREEN_NUMBER(SCREEN_COMMAND,ISCREEN,NSCREEN,IER)
      IF ( IER .EQ. 0 ) THEN
        CALL PU_GOTO_SCREEN(ISCREEN,IDX)
      ENDIF
  999 RETURN
      END
