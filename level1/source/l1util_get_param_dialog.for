      SUBROUTINE L1UTIL_GET_PARAM_DIALOG(VALUE, PROMPT, TRUE_MESSAGE,
     &  FALSE_MESSAGE)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Get a logical parameter interactively using COMPACK 
C-      routines.
C-
C-   Inputs  : VALUE    *MODIFIED* The variable to prompt for.
C-             PROMPT   The string to prompt the user with.
C-             TRUE_MESSAGE     The message to print if the variable is .TRUE.
C-             FALSE_MESSAGE    The message to print if the variable is .FALSE.
C-   Outputs : none
C-   Controls: none
C-
C-   Created   5-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL VALUE
      CHARACTER*(*) PROMPT, TRUE_MESSAGE, FALSE_MESSAGE
      CHARACTER*80  BUFFER
C
      CHARACTER*1 YN
C
      IF (VALUE .EQV. .TRUE.) THEN
        YN = 'Y'
      ELSE
        YN = 'N'
      ENDIF
C
      BUFFER = PROMPT // ' [' // YN // ']>'
      CALL GETPAR(1, BUFFER, 'U', YN)
      IF (YN .EQ. 'Y') VALUE = .TRUE.
      IF (YN .EQ. 'N') VALUE = .FALSE.
C
      IF (VALUE .EQV. .TRUE.) THEN
        BUFFER = ' ' // TRUE_MESSAGE
      ELSE
        BUFFER = ' ' // FALSE_MESSAGE
      ENDIF
      CALL INTMSG( BUFFER ) 
C
C----------------------------------------------------------------------
  999 RETURN
      END
