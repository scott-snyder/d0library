      SUBROUTINE L1COOR_ALLOC_SPECTRIG(SPEC_TRIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform necessary actions when a Specific Trigger is
C-     allocated.
C-
C-   Inputs  : SPEC_TRIG        The Specific Trigger number
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-SEP-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
C
      INTEGER SPEC_TRIG

C
      IF (ST_ALLOCATED(SPEC_TRIG) .EQV. .TRUE.) GOTO 999
      ST_ALLOCATED(SPEC_TRIG) = .TRUE.
C
C----------------------------------------------------------------------
  999 RETURN
      END
