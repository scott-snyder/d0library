      SUBROUTINE L1FW_UNPACK_ANDOR(NUM_ANDOR_USED, ANDOR_STATES,
     &  ANDOR_INDICES, ERROR)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Unpack Andor Terms as returned by a trigger
C-      subsystem.
C-
C-   Inputs  : NUM_ANDOR_USED   The number of Andor Terms this routine is
C-                              given.
C-             ANDOR_STATES     The state of each given Andor Term.
C-             ANDOR_INDICES    The corresponding Andor Term number for 
C-                              each given Andor Term.
C-   Outputs : none
C-   Controls: none
C-
C-   Created    4-SEP-1991 Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IMPLICIT NONE
      INCLUDE 'D0$PARAMS:LEVEL1_LOOKUP.PARAMS'
      INCLUDE 'D0$PARAMS:L1_CALTRIG.PARAMS'
      INCLUDE 'D0$PARAMS:L1_FRAMEWORK.PARAMS'
      INCLUDE 'D0$INC:L1FW_ANDOR_AND_MISC.INC'
      INTEGER  TRULEN
      EXTERNAL TRULEN
C
      INTEGER NUM_ANDOR_USED
      LOGICAL ANDOR_STATES(1:NUM_ANDOR)
      INTEGER ANDOR_INDICES(1:NUM_ANDOR)
      INTEGER ERROR
      CHARACTER*(*) STRING
      INTEGER SLEN
C
      INTEGER COUNT, COUNT2
C
C
      ERROR = UNPACK_ANDOR_OK
C
C       If the number of Andor Terms to unpack is out of range, then return
C         with an error.
      IF (NUM_ANDOR_USED .LT. 0) THEN
        ERROR = UNPACK_ANDOR_TOO_FEW
        GOTO 999
      ELSEIF (NUM_ANDOR_USED .GT. NUM_ANDOR) THEN
        ERROR = UNPACK_ANDOR_TOO_MANY
        GOTO 999
      ENDIF
C
C       If there are no Andor Terms to unpack, then return
      IF (NUM_ANDOR_USED .EQ. 0) GOTO 999
C
C       Check that the routine is not given more than one state for 
C       each Andor term
C
      DO COUNT = 1, NUM_ANDOR_USED
        DO COUNT2 = COUNT+1, NUM_ANDOR_USED
          IF (ANDOR_INDICES(COUNT) .EQ. ANDOR_INDICES(COUNT2)) THEN
            ERROR = UNPACK_ANDOR_CONFLICT_SELF
            GOTO 999
          ENDIF
        END DO
      END DO
C
C       Unpack each Andor Term        
      DO COUNT = 1, NUM_ANDOR_USED
C
C       Check that the term number is in range
        IF ((ANDOR_INDICES(COUNT) .LT. ANDOR_NUM_MIN) 
     &    .OR. (ANDOR_INDICES(COUNT) .GT. ANDOR_NUM_MAX)) THEN
          ERROR = UNPACK_ANDOR_INVALID
          GOTO 999
C
        ELSE
C
C       Check that each term is assigned a state only once
          IF (ANDOR_TERM_ASSIGNED(ANDOR_INDICES(COUNT)) 
     &      .EQV. .FALSE.) THEN
            ANDOR_TERM(ANDOR_INDICES(COUNT)) = ANDOR_STATES(COUNT)
            ANDOR_TERM_ASSIGNED(ANDOR_INDICES(COUNT)) = .TRUE.
          ELSE
            ERROR = UNPACK_ANDOR_CONFLICT_OTHER
            GOTO 999
          ENDIF
        ENDIF
      END DO
C        
C----------------------------------------------------------------------
  999 RETURN
C#######################################################################
      ENTRY L1FW_UNPACK_ANDOR_ERROR_STRING(ERROR, STRING, SLEN)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Return a string describing the given UNPACK_ANDOR_* 
C-                          error.
C-
C-   Inputs  : ERROR  The error status
C-   Outputs : STRING The error string
C-             SLEN   The length of STRING
C-   Controls: none
C-
C-   Created   8-OCT-1991   Level 1 Simulator, Michigan State University,
C-                      Philippe Laurens, Steven Klocek,
C-                      L1C -> L1SIM, and major upgrade to use LSM, COOR_sim
C----------------------------------------------------------------------
      IF (ERROR .EQ. UNPACK_ANDOR_OK) THEN
        STRING = 'No error'
      ELSEIF (ERROR .EQ. UNPACK_ANDOR_INVALID) THEN
        STRING = 'Invalid Andor Term Number'
      ELSEIF (ERROR .EQ. UNPACK_ANDOR_CONFLICT_SELF) THEN
        STRING = 'Andor Term Numbers confilct within subsystem'
      ELSEIF (ERROR .EQ. UNPACK_ANDOR_CONFLICT_OTHER) THEN
        STRING = 'Andor Term Number conflicts with other subsystem'
      ELSEIF (ERROR .EQ. UNPACK_ANDOR_TOO_MANY) THEN
        STRING = 'Too many Andor Terms'
      ELSEIF (ERROR .EQ. UNPACK_ANDOR_TOO_FEW) THEN
        STRING = 'Fewer than 0 Andor Terms'
      ELSE
        STRING = 'Unknown Error'
      ENDIF
C
      SLEN = TRULEN(STRING)
C
      RETURN
C----------------------------------------------------------------------
      END
