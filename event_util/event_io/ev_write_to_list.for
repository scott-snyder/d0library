      SUBROUTINE EV_WRITE_TO_LIST(NRUNS,RUNS,NEVTS_IN_RUNS,EVENT_NOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-         write events list to EVENT.LIST file
C-   Inputs  : 
C-     NRUNS          = number of runs
C-     RUNS           = list of runs
C-     NEVTS_IN_RUNS  = number of events in each run
C-     EVENT_NOS      = list of event numbers
C-
C-   Created   1-MAR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER NRUNS,RUNS(100),NEVTS_IN_RUNS(100),EVENT_NOS(200,100)
      INTEGER IUN,I,IER,K,IMAP(200),NEVTS,NDIF,IDIF,ADD
      INTEGER SORTED_EV(200,100),SORTED_NEVTS(100)
      LOGICAL FIRST,WRITE_RUN,OK
      SAVE IUN
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
      IF(FIRST.AND.NRUNS.GT.0) THEN
        CALL GTUNIT(111,IUN,IER)
        CALL D0OPEN(IUN,'EVENT_LIST','OF',OK)
        FIRST=.FALSE.
      ENDIF
C
C      sort runs in ascending order and corresponing arrays
      DO I=1,NRUNS
        IMAP(I)=I
      ENDDO
      CALL SRTINT(RUNS,NRUNS,IMAP)    
      DO I=1,NRUNS
        NEVTS=NEVTS_IN_RUNS(IMAP(I))
        SORTED_NEVTS(I)=NEVTS
        CALL UCOPY(EVENT_NOS(1,IMAP(I)),SORTED_EV(1,I),NEVTS)
      ENDDO
C
C       merge same runs
      IDIF=1
      DO I=2,NRUNS
        IF(RUNS(I).EQ.RUNS(IDIF)) THEN
          NEVTS=SORTED_NEVTS(IDIF)
          ADD  =SORTED_NEVTS(I)
          CALL UCOPY(SORTED_EV(1,I),SORTED_EV(NEVTS+1,IDIF),ADD)
          SORTED_NEVTS(IDIF)=NEVTS+ADD
        ELSE
          IDIF=IDIF+1
          NEVTS=SORTED_NEVTS(I)
          SORTED_NEVTS(IDIF)=NEVTS
          RUNS(IDIF)=RUNS(I)
          CALL UCOPY(SORTED_EV(1,I),SORTED_EV(1,IDIF),NEVTS)
        ENDIF
      ENDDO
C
C       sort events by run and write out
      NDIF=IDIF
      DO I=1,NDIF
        NEVTS=SORTED_NEVTS(I)
        CALL SRTINT(SORTED_EV(1,I),NEVTS,IMAP)    ! sort in ascending order
        IDIF=1
C           remove duplicate events
        DO K=2,NEVTS
          IF(SORTED_EV(K,I).NE.SORTED_EV(IDIF,I)) THEN
            IDIF=IDIF+1
            SORTED_EV(IDIF,I)=SORTED_EV(K,I)
          ENDIF
        ENDDO
C            write events out
        WRITE(IUN,*) RUNS(I),IDIF
        WRITE(IUN,*) (SORTED_EV(K,I),K=1,IDIF)
      ENDDO
  999 RETURN
      END
