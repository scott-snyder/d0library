      SUBROUTINE EV_ADD_TO_LIST(RUN,EVENT_NO)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      add to event list run and event number
C-      it can handle up to 100 runs with a maximum of 200 events/run
C-      or a total of 20000 events distributed among <100 runs
C-
C-   Inputs  : 
C-     RUN     = run number
C-     EVENT_NO= event number
C-
C-   ENTRY EV_WRITE_LIST : 
C-   calls EV_WRITE_TO_LIST to write event arrays to a file
C-    
C-   Created   1-MAR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN,EVENT_NO
      INTEGER NRUNS,RUNS(100),NEVTS_IN_RUNS(100),EVENT_NOS(200,100)
      INTEGER I,NEVT
      SAVE RUNS,NEVTS_IN_RUNS,EVENT_NOS,NRUNS
      LOGICAL ADD_EVT
      DATA NRUNS/0/
C----------------------------------------------------------------------
C
    1 CONTINUE
      IF(NRUNS.EQ.0) THEN
        DO I=1,100
          RUNS(I)=-1
          NEVTS_IN_RUNS(I)=0
        ENDDO
        NRUNS=1
        RUNS(1)=RUN
      ENDIF
C
      IF (RUN.NE.RUNS(NRUNS)) THEN
        IF(NRUNS.EQ.100) THEN         ! write everything out
          CALL EV_WRITE_TO_LIST(NRUNS,RUNS,NEVTS_IN_RUNS,
     &        EVENT_NOS)
          NRUNS=0
          GOTO 1     ! reuse arrays
        ENDIF
        NRUNS=NRUNS+1
        RUNS(NRUNS)=RUN
      ENDIF
      NEVT=NEVTS_IN_RUNS(NRUNS)+1
      ADD_EVT=NEVT.EQ.1
      IF(.NOT.ADD_EVT) ADD_EVT=EVENT_NO.NE.EVENT_NOS(NEVT-1,NRUNS)
      IF(ADD_EVT) THEN
        IF ( NEVT.GT.200 ) THEN
          NRUNS=NRUNS+1
          IF(NRUNS.GT.100) THEN
            NRUNS=0
            GOTO 1
          ENDIF
          RUNS(NRUNS)=RUN
          NEVT=1
        ENDIF
        EVENT_NOS(NEVT,NRUNS)=EVENT_NO
        NEVTS_IN_RUNS(NRUNS)=NEVT
      ENDIF
C
      RETURN
C
C
      ENTRY EV_WRITE_LIST
C
      CALL EV_WRITE_TO_LIST(NRUNS,RUNS,NEVTS_IN_RUNS,
     &        EVENT_NOS)
C
  999 RETURN
      END
