      SUBROUTINE L1_SPECIAL_TERMS_SIM(NUM_ANDOR_USED, ANDOR_STATES,
     &  ANDOR_INDICES)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return the state of the special purpose Andor Terms.
C-
C-      Andor Terms returned:
C-
C-         ZERO_BIAS : Always .TRUE.
C-
C-   Inputs  : none
C-   Outputs : NUM_ANDOR_USED   The number of Andor Terms being returned
C-             ANDOR_STATES     The states of the returned Andor Terms
C-             ANDOR_INDICES    The hardware index of each returned Andor
C-                                Term
C-   Controls: none
C-
C-   Created   7-NOV-1991 Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C-   Updated  25-NOV-1992   Philippe Laurens, Steven Klocek   
C-                      Simulate constant-state Andor Terms, given in L1SIM.RCP
C-   Updated   3-JUN-1993   Philippe Laurens - MSU L1 Trigger  
C-                      Implement andor terms copied from an
C-                      existing TRGR bank found in the input event
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$INC:ZEBCOM.INC'
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$PARAMS:L1SIM_CONTROL.PARAMS'
      INCLUDE 'D0$INC:L1SIM_CONTROL.INC'
      INCLUDE 'D0$INC:L1SIM_CONTROL_STRINGS.INC'
C
      LOGICAL L1UTIL_PICK_RESOURCE_RCP
      EXTERNAL L1UTIL_PICK_RESOURCE_RCP
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
      INTEGER ZERO_BIAS_TERM
      INTEGER IER
      INTEGER RCP_OK
      PARAMETER (RCP_OK = 0)
      INTEGER OUT_OF_RANGE
      PARAMETER (OUT_OF_RANGE = 1000)
      INTEGER FORCED_TERM_INDICES(1:NUM_ANDOR)
      INTEGER COPIED_TERM_INDICES(1:NUM_ANDOR)
      INTEGER INDEX, COUNT
      INTEGER LTRGR_LEVEL1 
C
      INTEGER   GZFIND_CRATE, GZTRGR
      EXTERNAL  GZFIND_CRATE, GZTRGR
C
      CHARACTER*70  ERR_MESSAGE
C
      LOGICAL FIRST
      SAVE FIRST, ZERO_BIAS_TERM, 
     &            FORCED_TERM_INDICES, COPIED_TERM_INDICES
      DATA FIRST / .TRUE. /
C
      NUM_ANDOR_USED = 0
      IF (FIRST .EQV. .TRUE.) THEN
        IF (L1UTIL_PICK_RESOURCE_RCP() .EQV. .FALSE.) GOTO 999
C
        CALL EZGET('ZERO_BIAS', ZERO_BIAS_TERM, IER)
        IF (IER .NE. RCP_OK) THEN
          ZERO_BIAS_TERM = OUT_OF_RANGE
        ENDIF
C
C       Get Andor Term indices for forced terms
C
        DO COUNT = 1, NUM_FORCED_TERMS
          CALL EZGET(FORCED_TERM_NAMES(COUNT), INDEX, IER)
          IF (IER .EQ. RCP_OK) THEN
            FORCED_TERM_INDICES(COUNT) = INDEX
          ELSE
            FORCED_TERM_INDICES(COUNT) = OUT_OF_RANGE
            ERR_MESSAGE = ' Andor Term not found in resource file ' 
     &        // FORCED_TERM_NAMES(COUNT)
            CALL ERRMSG( 'FORCED ANDOR NAME', 'L1_SPECIAL_TERMS_SIM',
     &                  ERR_MESSAGE, 'W')
          ENDIF
        END DO
C
C
C       Get Andor Term indices for copied terms
C
        DO COUNT = 1, NUM_COPIED_TERMS
          CALL EZGET( COPIED_TERM_NAMES(COUNT), INDEX, IER)
          IF (IER .EQ. RCP_OK) THEN
            COPIED_TERM_INDICES(COUNT) = INDEX
          ELSE
            COPIED_TERM_INDICES(COUNT) = OUT_OF_RANGE
            ERR_MESSAGE = ' Andor Term not found in resource file ' 
     &        // COPIED_TERM_NAMES(COUNT)
            CALL ERRMSG( 'COPIED ANDOR NAME', 'L1_SPECIAL_TERMS_SIM',
     &                  ERR_MESSAGE, 'W')
          ENDIF
        END DO
C
        CALL EZRSET()
C
        FIRST = .FALSE.
      ENDIF
C
      ANDOR_STATES(1) = .TRUE.
      ANDOR_INDICES(1) = ZERO_BIAS_TERM
C
C       Return 0 Andor Terms if none have been defined
C
      IF (ZERO_BIAS_TERM .NE. OUT_OF_RANGE) THEN
        NUM_ANDOR_USED = 1
      ELSE
        NUM_ANDOR_USED = 0
      ENDIF
C
C     take care of forced andor terms
C
      DO INDEX = 1, NUM_FORCED_TERMS
        IF (FORCED_TERM_INDICES(INDEX) .NE. OUT_OF_RANGE) THEN
          NUM_ANDOR_USED = NUM_ANDOR_USED + 1
          ANDOR_INDICES(NUM_ANDOR_USED) 
     &      = FORCED_TERM_INDICES(INDEX)
          ANDOR_STATES(NUM_ANDOR_USED) = FORCED_TERM_STATES(INDEX)
        ENDIF
      END DO
C
C     now take care of copied andor terms
C     first, find the Level 1 crate in the input event
C
      IF ( NUM_COPIED_TERMS .EQ. 0 ) GOTO 999
C
      LTRGR_LEVEL1 = GZFIND_CRATE ( 'TRGR', GZTRGR(), 11 )
C
      IF ( LTRGR_LEVEL1 .LE. 0 ) 
     &THEN 
        WRITE ( ERR_MESSAGE, 1000 )  LTRGR_LEVEL1 
 1000   FORMAT ( ' Cannot find L1 Crate for copied andor terms, code', 
     &            I4 )
        CALL ERRMSG( 'GZFIND_CRATE', 'L1_SPECIAL_TERMS_SIM',
     &                ERR_MESSAGE, 'W')
        GOTO 999
      ENDIF
C
      DO INDEX = 1, NUM_COPIED_TERMS
C
        IF (COPIED_TERM_INDICES(INDEX) .NE. OUT_OF_RANGE) 
     &  THEN
          NUM_ANDOR_USED = NUM_ANDOR_USED + 1
          ANDOR_INDICES(NUM_ANDOR_USED) = COPIED_TERM_INDICES(INDEX)
          CALL L1EXTRACT_ANDOR_TERM( IQ(LTRGR_LEVEL1),
     &                               COPIED_TERM_INDICES(INDEX),
     &                               ANDOR_STATES(NUM_ANDOR_USED) )
        ENDIF
      END DO
C
C----------------------------------------------------------------------
  999 RETURN
      END
