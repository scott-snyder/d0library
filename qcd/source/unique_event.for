      LOGICAL FUNCTION UNIQUE_EVENT(RUN,EVENT,NDUP)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For a given run, returns whether the particular
C_                         event has been already processed. 
C-                         Assumes that all files for a run are contiguously
C-                         processed!
C-
C-                         Each event number(up to 32000) has a associated bit 
C-       
C-
C-   Returned value  :     .TRUE.   = event not processed
C-                         .FALSE.  = event has been processed
C-
C-   Inputs  :              RUN = run number
C-                          EVENT = event number
C-   Outputs :              NDUP = number of duplicate events
C-   Controls: 
C-
C-   Created   8-FEB-1994   Brent J. May
C-   Updated   8-FEB-1994   Andrew G. Brandt only call error 1 time
C-   Updated  28-APR-1994   Andrew G. Brandt allow 320,000 events/run
C-   Updated  09-MAR-1996   Andrew G. Brandt EVENTSAVE L*4 to I*4 for SGI
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN, EVENT, EVENTBIT, EVENTWORD
      INTEGER RUNSAVE, NEVENTS, NDUP
      INTEGER*4 EVENTSAVE(0:10000) , BIT
      LOGICAL ERROUT
      DATA ERROUT /.FALSE./
      SAVE RUNSAVE, EVENTSAVE, NEVENTS
C----------------------------------------------------------------------
      UNIQUE_EVENT = .TRUE.
      IF (RUN.NE.RUNSAVE) THEN
        RUNSAVE=RUN
        CALL VZERO(EVENTSAVE,10001)
        NEVENTS = 0
        NDUP = 0
      ENDIF
      NEVENTS = NEVENTS + 1
      IF (EVENT.GT.320000) THEN
        IF(.NOT.ERROUT) THEN
          CALL ERRMSG('UNIQUE_EVENT','UNIQUE_EVENT',
     &         ' Event num > 320000 - not checking for duplicates','W')
          ERROUT=.TRUE.
        END IF
        GOTO 999
      ENDIF
C
C Check event bit to see if it exists
C
      EVENTWORD = EVENT/32      ! find word that event number is in
      EVENTBIT = MOD(EVENT,32)  ! find bit
      BIT = BTEST(EVENTSAVE(EVENTWORD),EVENTBIT)
      IF (BIT.ne.0) THEN        !event has been processed
        NDUP = NDUP + 1
        UNIQUE_EVENT = .FALSE.
      ELSE                      !set event bit
        EVENTSAVE(EVENTWORD) = IBSET(EVENTSAVE(EVENTWORD),EVENTBIT)
      ENDIF 
  999 RETURN
      END
