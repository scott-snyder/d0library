      SUBROUTINE L1UTIL_EXPAND_FILENAME(ORIGINAL, EXPANDED)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a complete translation of a file name
C-   (Translate all logicals, find the most recent version number, etc.)
C-   Uses PARSE_FILES and SEARCH_FILES from OFFLINE_UTIL.  If a matching file
C-   can not be found, then the origional filename is returned as the expanded
C-   file name. 
C-
C-      NOTE: The VMS routine which expands the file name can only handle file
C-      names no greater than 127 characters long (An arithmetic overflow trap
C-      occurs otherwise).  If a file name longer than 127 characters long is
C-      given, the original file name is returned.
C-
C-   Inputs  : ORIGINAL         The file name to be expanded
C-   Outputs : EXPANDED         The expanded file name
C-   Controls: none
C-
C-   Created  18-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) ORIGINAL, EXPANDED
      INTEGER IER
      LOGICAL NO_EXPANSION
      PARAMETER (NO_EXPANSION = .FALSE.)
      INTEGER MAX_NAME_LENGTH
      PARAMETER (MAX_NAME_LENGTH = 127)
C
C&IF VAXVMS
C
      IF ((LEN(ORIGINAL) .GT. MAX_NAME_LENGTH) 
     &  .OR. (LEN(EXPANDED) .GT. MAX_NAME_LENGTH)) THEN
        EXPANDED = ORIGINAL
        GOTO 999
      ENDIF
C
      IER = 0
      CALL PARSE_FILES(ORIGINAL, IER, NO_EXPANSION)
C
C       If there are any errors, return the original file name
      IF (IER .EQ. 0) THEN
        CALL SEARCH_FILES(EXPANDED, IER)
      ENDIF
      IF (IER .NE. 0) THEN
        EXPANDED = ORIGINAL
      ENDIF
C&ELSE
C&      EXPANDED = ORIGINAL
C&ENDIF
C----------------------------------------------------------------------
  999 RETURN
      END
