      SUBROUTINE L1_FRAMEWORK_SIM
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Calculate the states of the Specific Triggers.
C-
C-   Inputs  : common block variables
C-   Outputs : common block variables
C-   Controls: none
C-
C-   Created   6-SEP-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated   5-NOV-1991   Philippe Laurens, Steven Klocek
C-                      Added Support for Prescalers
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek
C-                      All Specific Trigger variables use [0,31] for indices
C-                      Moved Start Digitize calculation here from data block
C-                        fill routine.
C-                      Record when scalers are incremented.
C-                      Save the Spec Trig states after Level 1.
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek
C-                      delete ENTRY point L1FW_NUM_TRG_FIRED(NUM_FIRED)
C-                      update variable name FIRED_NUMBER -> NUM_SPTRG_PASS_L1
C-   Updated  28-SEP-1992   Philippe Laurens, Steven Klocek
C-                      Specific Trigger only fires when it is enabled.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$INC:L1C_GLOBAL_RESULTS.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:ZEBCOM.INC'
      REAL     RNDM
      EXTERNAL RNDM
C
      INTEGER TRIGGER, TYPE, BIT, SUB, ANDOR, IER, CMP, REF
      INTEGER GEO_SECT
      LOGICAL DIGITIZE
      INTEGER RCP_ERROR
      PARAMETER (RCP_ERROR = -1)
      LOGICAL ANDOR_PROGRAMMED
      CHARACTER*75 STRING
      INTEGER LTRGR_LEVEL1 
      INTEGER GZFIND_CRATE, GZTRGR
      EXTERNAL GZFIND_CRATE, GZTRGR
      INTEGER SPTRG_ANDOR_MASK, SPTRG_ENABLED_MASK 
C
      LTRGR_LEVEL1 = 0 
      BEAM_SCALER(1) = BEAM_SCALER(1) + 1
      BEAM_SCALER_INCREMENTED = 1
      NUM_SPTRG_PASS_L1 = 0
C
C       Find specific trigger states from AndOr term states.
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
C       If no Andor Terms have been assigned to a Specific Trigger, it is
C       considered to not be programmed.
        ANDOR_PROGRAMMED = .FALSE.
C       The AndOr network enforces the logical AND of several terms,
C       So assume the andor fired, and look for reasons it shouldn't have.
        FSTD_ANDOR_FIRED(TRIGGER) = .TRUE.
C
C       All Andor terms allocated to this Specific Trigger must have the
C       specified state for this trigger to fire.
        DO ANDOR = ANDOR_NUM_MIN, ANDOR_NUM_MAX
          IF (SPECTRIG_ANDOR_ALLOC(ANDOR, TRIGGER) .EQV. .TRUE.) THEN
C
C           Make sure that the input terms have all been given states
C
            IF (ANDOR_TERM_ASSIGNED(ANDOR) .EQV. .FALSE.) THEN
              CALL ERRMSG('SP TRIG USES UNKNOWN AO STATE',
     &          'L1_FRAMEWORK_SIM',
     &          'Specific Trigger uses an Andor Term which ' //
     &          'was never given a state for this event', 'W')
              CALL ERRMSG('SP TRIG USES UNKNOWN AO HINT',
     &          'L1_FRAMEWORK_SIM',
     &          'Using MC but set up for Data or vice versa?' //
     &          ' Check L1SIM.RCP, programming, resource files','W')
              WRITE (STRING,100) TRIGGER, ANDOR
              CALL ERRMSG('SP TRIG USES UNKNOWN AO STA ID',
     &          'L1_FRAMEWORK_SIM', STRING, 'F')
  100         FORMAT( 'Specific Trigger: ',I4,' Andor Term: ', I4 )
              GOTO 999
            ENDIF
C
            ANDOR_PROGRAMMED = .TRUE.
            IF ( ANDOR_TERM(ANDOR)
     &        .NEQV. SPECTRIG_ANDOR_POLARITY(ANDOR, TRIGGER)) THEN
              FSTD_ANDOR_FIRED(TRIGGER) = .FALSE.
            ENDIF
          ENDIF
        END DO
C
        PROGRAMMED_TRIGGER(TRIGGER) = .TRUE.
        IF ( ANDOR_PROGRAMMED .EQV. .FALSE. ) THEN
          PROGRAMMED_TRIGGER(TRIGGER) = .FALSE.
C         The default behavior for the andor decision of a Specific Trigger that
C         was not programmed is defined as not to fire
          FSTD_ANDOR_FIRED(TRIGGER) = .FALSE.
C
C         If a Specific Trigger is not programmed it shouldn't be enabled.
          ST_ENABLED(TRIGGER) = .FALSE.
C         Note the somewhat backwards presetting in L1_FW_AND_CT_CLEAR where all
C         sptrg are set enabled while COOR can override it with the ENABLE
C         message to enable/deisable some sptrg.
C         With real (complete) programming files, COOR always sends the ENABLE
C         messages after the definition of the triggers. And at some point
C         in the configuration, more sptrg might be defined than are enabled.
C         But no enable message is created in the direct output of COOR_SIM.
C         Defaulting to an enabled state as soon as the sptrg is defined allows
C         L1SIM to run off the output of COOR_SIM.
C         This compromise lets L1SIM use either COOR_SIM input or archived
C         messages. But one can think of some stream of real messages that would
C         not produce the expected state. This compromise should still do the
C         right thing in all normal cases.
        END IF
C
C        Now let's consider the enable state of the specific Trigger.
        FSTD_ENABLED(TRIGGER) = .TRUE.
C
C        COOR may have disabled the specific trigger
        IF ( ST_ENABLED(TRIGGER) .EQV. .FALSE.) THEN
          FSTD_ENABLED(TRIGGER) = .FALSE.
C
C        The Front-End busy or the Level 2 might disable the specific trigger
C        (neither one is implemented in L1SIM as of 14-FEB-1994)
        ELSE IF ( ST_FSTD_VETO(TRIGGER) .EQV. .TRUE. ) THEN
          FSTD_ENABLED(TRIGGER) = .FALSE.
C
C        The Prescaler might be preventing the Specific Trigger from firing.
        ELSE IF ( (APPLY_PRESCALER .EQV. .TRUE.)
     &      .AND. (ST_PRESCALER(TRIGGER) .GT. 1) ) THEN
C
C          For Specific Triggers whose andor did not fire, it is not possible to
C          properly recreate the effect of the prescaler on the FSTD enabling
C          status.
C          but more often than not, if a sptrg uses the prescaler and its andor
C          didn't fire, the prescaler is also disabling it.
C
          IF ( FSTD_ANDOR_FIRED(TRIGGER) .EQV. .FALSE. ) THEN
            FSTD_ENABLED(TRIGGER) = .FALSE.
C
C          When the andor fired, and the specific trigger is prescaled, L1SIM
C          may be globally programmed for one of three prescaler modes
C                 1) ignore all prescalers
C                 2) apply the prescale ratios as a random 1 of N filter
C                 3) look in the input event and take the same firing decision
C
          ELSE IF ( PRESCALER_USE_EVENT_DATA .EQV. .TRUE. ) THEN
C
C            This is the mode that follows the input data. This mode is
C            useful when trying to reproduce and check the trigger decisions
C            in Level 1 and then Level 2.
C            This mode implies that there already was a TRGR bank (and an L1
C            Crate) in the input event.
C
C            Read information from the L1 CRATE 
C            (but only once, use LTRGR_LEVEL1 that has been zeroed)
            IF ( LTRGR_LEVEL1 .EQ. 0 ) THEN
              LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
              IF ( LTRGR_LEVEL1 .LE. 0 ) THEN
                CALL ERRMSG( 'NO L1 CRATE BANK IN INPUT EVENT',
     &                       'L1_FRAMEWORK_SIM',
     &                       'Mode PRESCALER_USE_EVENT_DATA needs ' //
     &                       'existing L1 Data in input events',
     &                       'W')
C                pretend all trigger andor fired, and enabled
                SPTRG_ANDOR_MASK   = - 1
                SPTRG_ENABLED_MASK = - 1
              ELSE
                CALL L1EXTRACT_ANDOR_FIRED  ( IQ(LTRGR_LEVEL1),
     &                                        SPTRG_ANDOR_MASK )
                CALL L1EXTRACT_SPTRG_ENABLED( IQ(LTRGR_LEVEL1),
     &                                        SPTRG_ENABLED_MASK )
              END IF
            END IF
C
C            First, apply the sanity check requiring that the Specific Trigger
C            must have had its andor fired in the input event also.
            IF ( BTEST( SPTRG_ANDOR_MASK, TRIGGER) .NEQV. .TRUE. ) THEN
              WRITE (STRING,200) TRIGGER
  200         FORMAT( 'INCONSISTENT ANDOR FIRING - SPTRG #', I3 )
              CALL ERRMSG( STRING,
     &                    'L1_FRAMEWORK_SIM',
     &                    'PRESCALER_USE_EVENT_DATA: ' //
     &                    'sptrg andor fired in simu, ' //
     &                    'but did not in input data ->' //
     &                    'Check programming ',
     &                    'W')
            ELSE
C
C              We can now see whether the specific trigger was enabled in the
C              input event, and do the same thing.
              IF ( BTEST( SPTRG_ENABLED_MASK, TRIGGER) .NEQV. .TRUE. )
     &           FSTD_ENABLED(TRIGGER) = .FALSE.
            END IF
C
          ELSE
C            This is the mode where we try to simulate the 1 of N prescaling.
C            The hardware prescalers are implemented with counters that must
C            reach their programmed count before an andor fired signal can be
C            converted into a sptrg fired. The software uses a random 1 of N
C            selection to avoid synchronous effects.
C            (TRIGGER is passed as argument to RNDM to prevent the optimizer to
C            play tricks on us and like take the call out of the do loop).
C
            IF ( RNDM(TRIGGER) * ST_PRESCALER(TRIGGER) .GE. 1 )
     &             FSTD_ENABLED(TRIGGER) = .FALSE.
          END IF
C
        END IF
C
        IF ( FSTD_ENABLED(TRIGGER) .EQV. .TRUE. ) THEN
          ENABLE_SCALERS(1,TRIGGER) = ENABLE_SCALERS(1,TRIGGER) + 1
          ENABLE_SCALERS_INCREMENTED(TRIGGER) = 1
        ELSE
          ENABLE_SCALERS_INCREMENTED(TRIGGER) = 0
        END IF
C
        IF ( ( FSTD_ANDOR_FIRED(TRIGGER) .EQV. .TRUE. )
     &    .AND. ( FSTD_ENABLED(TRIGGER) .EQV. .TRUE.) ) THEN
          FIRED_TRIGGER(TRIGGER) = .TRUE.
          NUM_SPTRG_PASS_L1 = NUM_SPTRG_PASS_L1 + 1
          FIRED_SCALERS(1,TRIGGER) = FIRED_SCALERS(1,TRIGGER) + 1
          FIRED_SCALERS_INCREMENTED(TRIGGER) = 1
        ELSE
          FIRED_TRIGGER(TRIGGER) = .FALSE.
          FIRED_SCALERS_INCREMENTED(TRIGGER) = 0
        END IF
C
      END DO
C
C     If any of the specific triggers fired, we have a global trigger
C
      IF(NUM_SPTRG_PASS_L1.NE.0) THEN
        TRIGGER_SCALER(1) = TRIGGER_SCALER(1) + 1
        TRIGGER_SCALER_INCREMENTED = 1
      ELSE
        TRIGGER_SCALER_INCREMENTED = 0
      ENDIF
C
C       Save the Level 1 Spec Trig states
C
      DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
        ST_LEVEL1_STATE(TRIGGER) = FIRED_TRIGGER(TRIGGER)
      END DO
C
C       Determine Start Digitize states
C
      DO GEO_SECT = GEO_NUM_MIN, GEO_NUM_MAX
        DIGITIZE = .FALSE.
        DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
          IF ((ST_STARTDGT(GEO_SECT, TRIGGER) .EQV. .TRUE.)
     &        .AND. (FIRED_TRIGGER(TRIGGER) .EQV. .TRUE.)) THEN
            DIGITIZE = .TRUE.
          ENDIF
        END DO
        GS_STARTDGT(GEO_SECT) = DIGITIZE
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
