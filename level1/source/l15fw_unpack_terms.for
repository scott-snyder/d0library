      SUBROUTINE L15FW_UNPACK_TERMS(NUM_TERMS_USED, TERM_STATES,
     &  TERM_INDICES, ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Level 1.5 terms returned by a Trigger
C-      subsystem.
C-
C-   Inputs  : NUM_TERMS_USED   The number of Level 1.5 terms given to this
C-                              routine.
C-             TERM_STATES      The state of each given Level 1.5 term.
C-             TERM_INDICES     The indices of each given Level 1.5 term.
C-   Outputs : ERROR            Error status. .TRUE. on error.
C-   Controls: none
C-
C-   Created   3-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      INTEGER NUM_TERMS_USED
      LOGICAL TERM_STATES(1:*)
      INTEGER TERM_INDICES(1:*)
      LOGICAL ERROR
C
      INTEGER INDEX
      INTEGER TERM 
C
      DO INDEX = 1, NUM_TERMS_USED
        TERM = TERM_INDICES(INDEX)
        IF (L15_TERM_ASSIGNED(TERM) .EQV. .TRUE.) THEN
          ERROR = .TRUE.
          GOTO 999
        ENDIF
        L15_TERM_STATE(TERM) = TERM_STATES(INDEX)
        L15_TERM_ASSIGNED(TERM) = .TRUE.
      END DO
C
      ERROR = .FALSE.
C----------------------------------------------------------------------
  999 RETURN
      END
