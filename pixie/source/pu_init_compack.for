      SUBROUTINE PU_INIT_COMPACK
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Setup STATUS and SPLIT screen windows for
C-   PIXIE applications.
C-
C-   Inputs  : None
C-   Outputs : None
C-   Controls:
C-
C-   Created  19-MAY-1990   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL FIRST
      INTEGER STATUS_SCREEN_LINES
      PARAMETER( STATUS_SCREEN_LINES = 3 )
C
      INTEGER SPLIT_SCREEN_LINES
      PARAMETER( SPLIT_SCREEN_LINES  = 6 )
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF ( FIRST ) THEN
        CALL SETLIN(SPLIT_SCREEN_LINES)
        CALL SETSTA(STATUS_SCREEN_LINES)
        CALL SPLSTA
        CALL SPLTIT
        CALL STAMSG(' System Display PIXIE',.TRUE.)
        FIRST = .FALSE.
      ENDIF
  999 RETURN
      END
