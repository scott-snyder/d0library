      LOGICAL FUNCTION CD_PULSER_RESET()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : RESET PULSER
C-
C-   Returned value  :
C-   Inputs  :
C-   Outputs :
C-   Controls:
C-
C-   Created  21-NOV-1990   Susan K. Blessing
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER I
      INTEGER ILEN,CALL_STATUS
      CHARACTER*50 STRING
      LOGICAL OK_OUT
C
      INCLUDE 'D0$INC:COOR_INFO.INC'
      INCLUDE 'D0$INC:ZPULSER.INC'
C
C----------------------------------------------------------------------
C
      CD_PULSER_RESET = .TRUE.
C
      IF (NPULSE.EQ.0) GO TO 999
C
      CALL INTMSG(' ')
      CALL INTMSG(' Resetting pulser.')
C
C  Attach to Coor
C
      CALL DO_COOR_ATTACH
      IF (.NOT.CONNECT_MADE) THEN
        CALL INTMSG(' Unable to attach to COOR to reset pulser.')
        GO TO 999
      END IF
C
      DO I = 1, NPULSE
        TARGET = PULSER(I)
        CALL STR$TRIM(TARGET,TARGET,ILEN)
C GET OWNERSHIP
        CALL GET_PLS(TARGET,CALL_STATUS)
C
        IF (CALL_STATUS.EQ.0) THEN
          AMPLTDA = 0                 ! Zero out all parameters
          AMPLTDB = 0
          SHHALF = 0
          SHCARD = 0
          PREAMP = 0
          POLARITY = 0
          QUADRANT = 0
          CRTSEL = 0                  ! Deselect shaper crate
          CALL ZSET_PULSER            ! Set the Pulser with zero's
          CALL DROP_RESOURCES(CALL_STATUS)
        ELSE
          WRITE(STRING,10) TARGET
   10     FORMAT('  Unable to obtain ownership of pulser ',A12,
     &      '.  Pulser not reset.')
          CALL INTMSG(STRING)
        ENDIF
      END DO
C
      CALL DETACH_COOR(OK_OUT)
C
  999 RETURN
      END
