      SUBROUTINE L1_AND_L15_RUN_STATS
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Print the run statistics
C-
C-   Inputs  : none
C-   Outputs : file output
C-   Controls: none
C-
C-   Created  10-DEC-1991   Philippe Laurens, Steven Klocek
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      INTEGER LUN,SSUNIT
C
      REAL PERCENT
      INTEGER A, B
      INTEGER TRIGGER
      INTEGER ANDOR
      LOGICAL TRIGGER_UNUSED
C
      PERCENT(A, B) = (FLOAT(A) / FLOAT(MAX(B,1))) * 100.0
C
C----------------------------------------------------------------------
      LUN = SSUNIT ()
C
C
   90 FORMAT('1')
      WRITE(LUN, 90)
C
  100 FORMAT( ' ', 78('-'))
      WRITE(LUN,100) 
      WRITE(LUN,*)
C
      WRITE(LUN,*) 'Level 1 and Level 1.5 Run Statistics'
      WRITE(LUN,*) '------------------------------------'
      WRITE(LUN,*)
C
C       Print out global statistics
C
  120 FORMAT(' ', A, T56, ':', I6, :, '/', I6, ' = ', F5.1, ' %')
      WRITE(LUN,120) 'Events Processed', COUNT_EVENTS_PROCESSED
      WRITE(LUN,*)
C
      WRITE(LUN,*) 'Global Statistics'
      WRITE(LUN,*) '-----------------'
      WRITE(LUN,*)
C
C
      WRITE(LUN,120) 'Events Passed by Level 1 and Level 1.5', 
     &  COUNT_EVENTS_PASSED, COUNT_EVENTS_PROCESSED,
     &  PERCENT(COUNT_EVENTS_PASSED, COUNT_EVENTS_PROCESSED)
      WRITE(LUN,*)
C
      WRITE(LUN,120) 'Pure Level 1 Events Passed', COUNT_EVENT_PURE_L1,
     &  COUNT_EVENTS_PROCESSED,  
     &  PERCENT(COUNT_EVENT_PURE_L1, COUNT_EVENTS_PROCESSED)
      WRITE(LUN,*)
C
      WRITE(LUN,120) 'Potential Events For Level 1.5',
     &  COUNT_EVENT_L15_POTENTIAL, COUNT_EVENTS_PROCESSED,
     &  PERCENT(COUNT_EVENT_L15_POTENTIAL, COUNT_EVENTS_PROCESSED)
C
      WRITE(LUN,120) 'Events Passed With Level 1.5 Decision Skipped',
     &  COUNT_EVENT_L15_SKIPPED, COUNT_EVENT_L15_POTENTIAL,
     &  PERCENT(COUNT_EVENT_L15_SKIPPED, COUNT_EVENT_L15_POTENTIAL)
C
      WRITE(LUN,120) 'Events Submitted to Level 1.5',
     &  COUNT_EVENT_L15_SUBMITTED, COUNT_EVENT_L15_POTENTIAL,
     &  PERCENT(COUNT_EVENT_L15_SUBMITTED, COUNT_EVENT_L15_POTENTIAL)
C
      WRITE(LUN,120) 'Events Confirmed by Level 1.5',
     &  COUNT_EVENT_L15_PASSED, COUNT_EVENT_L15_SUBMITTED,
     &  PERCENT(COUNT_EVENT_L15_PASSED, COUNT_EVENT_L15_SUBMITTED)
C
      WRITE(LUN,120) 'Events Rejected by Level 1.5',
     &  COUNT_EVENT_L15_REJECT, COUNT_EVENT_L15_SUBMITTED,
     &  PERCENT(COUNT_EVENT_L15_REJECT, COUNT_EVENT_L15_SUBMITTED)
C
      WRITE(LUN,*)
C
C       Print out information by Specific Trigger
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        TRIGGER_UNUSED = .TRUE.
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          IF (SPECTRIG_ANDOR_ALLOC(ANDOR, TRIGGER) .EQV. .TRUE.) THEN
            TRIGGER_UNUSED = .FALSE.
          ENDIF
        END DO
C
  130   FORMAT(' Specific Trigger #',I2, ': ', A)
  140   FORMAT(' --------------------')
        IF (TRIGGER_UNUSED .EQV. .TRUE.) THEN
          WRITE (LUN,130) TRIGGER, 'Unused'
          WRITE (LUN,140)
          WRITE (LUN,*)
C
        ELSE IF (ST_IS_L15(TRIGGER) .EQV. .FALSE.) THEN
          WRITE (LUN,130) TRIGGER, 'Pure Level 1'
          WRITE (LUN,140)
          WRITE(LUN,120) 'Total Events Passed',
     &      COUNT_ST_PASSED(TRIGGER), COUNT_EVENTS_PROCESSED,
     &      PERCENT(COUNT_ST_PASSED(TRIGGER), COUNT_EVENTS_PROCESSED)
          WRITE (LUN,*)
C
        ELSE ! Is a Level 1.5 Trigger
          WRITE (LUN,130) TRIGGER, 'Uses Level 1.5'
          WRITE(LUN,140)
C
          WRITE(LUN,120) 'Potential Events For Level 1.5',
     &      COUNT_ST_L15_POTENTIAL(TRIGGER), COUNT_EVENTS_PROCESSED,
     &      PERCENT(COUNT_ST_L15_POTENTIAL(TRIGGER), 
     &      COUNT_EVENTS_PROCESSED)
C
          WRITE(LUN,120) 
     &      'Events Passed With Level 1.5 Decision Skipped',
     &      COUNT_ST_L15_SKIPPED(TRIGGER), 
     &      COUNT_ST_L15_POTENTIAL(TRIGGER),
     &      PERCENT(COUNT_ST_L15_SKIPPED(TRIGGER), 
     &              COUNT_ST_L15_POTENTIAL(TRIGGER))
C
          WRITE(LUN,120) 'Events Submitted to Level 1.5',
     &      COUNT_ST_L15_SUBMITTED(TRIGGER), 
     &      COUNT_ST_L15_POTENTIAL(TRIGGER),
     &      PERCENT(COUNT_ST_L15_SUBMITTED(TRIGGER), 
     &              COUNT_ST_L15_POTENTIAL(TRIGGER))
C
          WRITE(LUN,120) 'Events Confirmed by Level 1.5',
     &      COUNT_ST_L15_PASSED(TRIGGER),
     &      COUNT_ST_L15_SUBMITTED(TRIGGER),
     &      PERCENT(COUNT_ST_L15_PASSED(TRIGGER),
     &              COUNT_ST_L15_SUBMITTED(TRIGGER))
C
          WRITE(LUN,120) 'Events Rejected by Level 1.5',
     &      COUNT_ST_L15_REJECT(TRIGGER),
     &      COUNT_ST_L15_SUBMITTED(TRIGGER),
     &      PERCENT(COUNT_ST_L15_REJECT(TRIGGER),
     &              COUNT_ST_L15_SUBMITTED(TRIGGER))
C
          WRITE(LUN,120) 'Total Events Passed',
     &      COUNT_ST_PASSED(TRIGGER), COUNT_EVENTS_PROCESSED,
     &      PERCENT(COUNT_ST_PASSED(TRIGGER), COUNT_EVENTS_PROCESSED)
C
          WRITE(LUN,*)
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
