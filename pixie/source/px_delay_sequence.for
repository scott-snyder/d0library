      SUBROUTINE PX_DELAY_SEQUENCE(COMMAND)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : waits a certain amount of second before going
C-   on displaying the next sequence display.
C-
C-   Inputs  : COMMAND [C*]: Comamnd containing the delay amount
C-   
C-   Outputs : None
C-
C-   Created  25-SEP-1991   Lupe Howell
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER*(*) COMMAND
C
      REAL    DELAY,VALUE
      INTEGER I,J,K
C----------------------------------------------------------------------
      DELAY = VALUE(COMMAND,I,J,K)
      IF ( DELAY .LE. 0 ) THEN
        DELAY = 10.00
      ENDIF
      CALL WAITIT(DELAY)
  999 RETURN
      END
