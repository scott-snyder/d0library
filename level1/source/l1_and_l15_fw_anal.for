      SUBROUTINE L1_AND_L15_FW_ANAL
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Record run statistic information on each event for
C-     later inclusion in the standard summary file.
C-     NOTE: This should be called before L1_CALTRIG_ANAL.
C-
C-   Inputs  : none
C-   Outputs : none
C-   Controls: none
C-
C-   Created  13-DEC-1991   Philippe Laurens, Steven Klocek
C-                      Created from parts of L1_FW_AND_CT_ANAL and
C-                        L15_FRAMEWORK_ANAL.
C-   Updated   3-JAN-1992   Philippe Laurens, Steven Klocek
C-                      All Specific Trigger variables use [0,31] for indices
C-   Updated  24-FEB-1992   Philippe Laurens, Steven Klocek
C-                      add weighted trigger hits
C-   Updated   8-MAR-1992   Philippe Laurens, Steven Klocek  
C-                      update to use added NUM_*_SPTRG_* variables
C-                      remove obsolete L1FW_NUM_TRG_FIRED, L15_CONFIRMED 
C-   Updated  17-AUG-1992   Philippe Laurens, Steven Klocek  
C-                      Record whether Accept and Reject scalers incremented.
C-   Updated  17-NOV-1992   Philippe Laurens, Steven Klocek  
C-                      Moved calculation of Level 1.5 Scalers to
C-                      L15_FRAMEWORK_SIM. 
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:LEVEL1_LOOKUP.INC'
      INCLUDE 'D0$INC:L1_SPECIFIC_TRIGGER.INC'
      INCLUDE 'D0$INC:L1C_EVENT_RAW.INC'
      INCLUDE 'D0$INC:L1C_EVENT.INC'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INCLUDE 'D0$PARAMS:L15_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L15_FRAMEWORK.INC'
C
      INTEGER TRIGGER
      INTEGER NUM_TRIGGERS_FIRED
      LOGICAL IS_POTENTIAL_L15
      LOGICAL POTENTIAL_L15_SKIPPED
      LOGICAL ANY_L15_ST_FIRED
C
      LOGICAL FIRST
      SAVE FIRST
      DATA FIRST /.TRUE./
C
C       INITIALIZATION PART
      IF ( FIRST ) THEN
        FIRST = .FALSE.
        CALL HCDIR ('//PAWC',' ')       ! GO TO TOP DIRECTORY
        CALL HMDIR ('L1SIM','S')          ! CREATE L1SIM DIRECTORY
C
        CALL HBOOK1 (1000, ' TRIGGER HITS$', 32, 0., 32., 0)
        CALL HBOOK1 (1001, ' Weighted Trigger Hits$', 32, 0., 32., 0)
        CALL HBOOK1 (1005,
     &    ' Level 1.5 Triggers Submitted to Level 1.5$',
     &    32, 0., 32., 0)
C
      END IF
C
      CALL HCDIR('//PAWC/L1SIM',' ')  ! go to CTTR directory
C
      COUNT_EVENTS_PROCESSED = COUNT_EVENTS_PROCESSED + 1
C
C       Record Trigger statistics related to whole system = Level 1 + Level 1.5
C       ------------------------------------------------------------------------
C
      IF ( NUM_SPTRG_PASS_L1SIM .NE. 0 ) THEN
C
        COUNT_EVENTS_PASSED = COUNT_EVENTS_PASSED + 1
C
        DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
          IF ( FIRED_TRIGGER(TRIGGER) .EQV. .TRUE. ) THEN
C
            COUNT_ST_PASSED(TRIGGER) = COUNT_ST_PASSED(TRIGGER) + 1
C
C               Record for Trigger firing histogram
C
            CALL HF1 (1000,FLOAT(TRIGGER),1.)
            CALL HF1 (1001,FLOAT(TRIGGER),ISAJET_WG)
C
          END IF
        END DO
      END IF
C
C       Record Trigger statistics related to Level 1
C       --------------------------------------------
C
      IF ( NUM_SPTRG_PASS_L1 .NE. 0 ) THEN
C
        IF ( NUM_L15_SPTRG_PASS_L1 .EQ. 0 )
     &      COUNT_EVENT_PURE_L1 = COUNT_EVENT_PURE_L1 + 1
C
      END IF
C
C       Record Trigger statistics related to Level 1.5
C       ----------------------------------------------
C
      IF ( NUM_L15_SPTRG_PASS_L1 .NE. 0 ) THEN
C
        DO TRIGGER = TRG_NUM_MIN, TRG_NUM_MAX
C
          IF ( L15_ST_FIRED_AT_L1(TRIGGER) .EQV. .TRUE. ) THEN
C
            COUNT_ST_L15_POTENTIAL(TRIGGER)
     &        = COUNT_ST_L15_POTENTIAL(TRIGGER) + 1
C
            IF ( NUM_L15_SPTRG_SENT_L15 .NE. 0 ) THEN
C
              COUNT_ST_L15_SUBMITTED(TRIGGER)
     &          = COUNT_ST_L15_SUBMITTED(TRIGGER) + 1
C
              IF ( FIRED_TRIGGER(TRIGGER) .EQV. .TRUE. ) THEN
                COUNT_ST_L15_PASSED(TRIGGER)
     &            = COUNT_ST_L15_PASSED(TRIGGER) + 1
              ELSE
                COUNT_ST_L15_REJECT(TRIGGER)
     &            = COUNT_ST_L15_REJECT(TRIGGER) + 1
              END IF
C
C               Histogram of Trigger Firings which add dead time
C
              CALL HF1(1005, FLOAT(TRIGGER), 1.)
C
            ELSE
C
              COUNT_ST_L15_SKIPPED(TRIGGER)
     &          = COUNT_ST_L15_SKIPPED(TRIGGER) + 1
C
            END IF
          END IF
        END DO
      END IF
C
      CALL HCDIR ('//PAWC',' ')       ! LEAVE HBOOK IN TOP DIRECTORY
C----------------------------------------------------------------------
  999 RETURN
      END
