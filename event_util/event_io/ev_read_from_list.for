      SUBROUTINE EV_READ_FROM_LIST(RUN,NEVTS,EVENT_NOS)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     given run# read list of events
C-   Inputs  : 
C-     RUN = run number
C-   Outputs : 
C-     NEVTS= number of events on list
C-     EVENT_NOS= event ids on list
C-
C-   Created   1-MAR-1991   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER RUN,NEVTS,EVENT_NOS(*)
      INTEGER NEVT_READ,RUN_READ,IUN,IER,K
      LOGICAL OK
C----------------------------------------------------------------------
      NEVTS=0
      CALL GTUNIT(111,IUN,IER)
      CALL D0OPEN(IUN,'EVENT_LIST','IF',OK)
    2 READ(IUN,*,END=999) RUN_READ,NEVT_READ
      READ(IUN,*) (EVENT_NOS(K),K=1,NEVT_READ)
      IF(RUN_READ.NE.RUN) GOTO 2
      NEVTS=NEVT_READ
      CLOSE(IUN)
      CALL RLUNIT(111,IUN,IER)
  999 RETURN
      END
