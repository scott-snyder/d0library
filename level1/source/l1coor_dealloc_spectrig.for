      SUBROUTINE L1COOR_DEALLOC_SPECTRIG(SPEC_TRIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform necessary functions when a Specific Trigger
C-      is deallocated.
C-
C-   Inputs  : SPEC_TRIG        The number of the Specific Trigger
C-   Outputs : none
C-   Controls: none
C-
C-   Created  24-SEP-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
C
      INTEGER SPEC_TRIG
      INTEGER ANDOR, GEO_SECT
C
      DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
        SPECTRIG_ANDOR_ALLOC(ANDOR, SPEC_TRIG) = .FALSE.
        SPECTRIG_ANDOR_POLARITY(ANDOR, SPEC_TRIG) = .FALSE.
      END DO
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        ST_STARTDGT(GEO_SECT, SPEC_TRIG) = .FALSE.
      END DO
      ST_ALLOCATED(SPEC_TRIG) = .FALSE.
      ST_ENABLED(SPEC_TRIG) = .FALSE.
      ST_PRESCALER(SPEC_TRIG) = 0
      OBEY_FEBUSY(SPEC_TRIG) = .FALSE.
      OBEY_L2BUSY(SPEC_TRIG) = .FALSE.
C
      ENTRY L1COOR_CLEAR_SPECTRIG_SCALERS(SPEC_TRIG)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Just clear the scalers associated with a Specific
C-        Trigger.
C-
C-   Inputs  : SPEC_TRIG  The number of the Specific Trigger
C-   Outputs : none
C-   Controls: none
C-
C-   Created  28-SEP-1992   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      FIRED_SCALERS(1,SPEC_TRIG) = 0
      FIRED_SCALERS(2,SPEC_TRIG) = 0
      FIRED_SCALERS_INCREMENTED(SPEC_TRIG) = 0
      ENABLE_SCALERS(1, SPEC_TRIG) = 0
      ENABLE_SCALERS(2, SPEC_TRIG) = 0
      ENABLE_SCALERS_INCREMENTED(SPEC_TRIG) = 0
C
C----------------------------------------------------------------------
  999 RETURN
      END
