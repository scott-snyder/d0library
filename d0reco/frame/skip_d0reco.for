      FUNCTION SKIP_D0RECO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-      Skip over a set of events
C-   Returned value  : true if processing of event is to be skipped
C-
C-   Created  14-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL SKIP_D0RECO
      INTEGER MAX_SKIPS
      PARAMETER (MAX_SKIPS=10)
      INTEGER NUMBER_OF_SKIPS     ! number of skips requested
      INTEGER SKIPS_DONE          !         "       done
      INTEGER NSKIP,IER,RUN,ID,EVTCNT
C
C   list of event ids marking ranges to skip
C   4 numbers/range: run number(RUN) and output event number(ID) for start
C                              "               "                 for end
C        if start run number is .lt. 0 assume NUMBER_OF_SKIPS means
C        consecutive number of events starting from first event on file
C    to skip only one specific event start and end numbers must be identical
      INTEGER EVENTS_TO_SKIP(4,MAX_SKIPS)
      CHARACTER*60 MSG
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      SKIP_D0RECO=.FALSE.
      IF(FIRST) THEN
        CALL EZPICK('D0RECO_RCP')
        CALL EZGET('NUMBER_OF_SKIPS',NUMBER_OF_SKIPS,IER)
        FIRST=.FALSE.
        SKIPS_DONE=0
        IF(NUMBER_OF_SKIPS.LE.0) GOTO 999
C
        IF(NUMBER_OF_SKIPS.LE.MAX_SKIPS) THEN
          CALL EZGET_iarr('EVENTS_TO_SKIP',EVENTS_TO_SKIP,IER) ! get list of events
          IF(IER.NE.0) EVENTS_TO_SKIP(1,1)=0
        ELSE
          WRITE(MSG,100) MAX_SKIPS
          CALL ERRMSG('D0RECO','SKIP_D0RECO',MSG,'W')
          CALL EZRSET
          GOTO 999                 ! failed
        ENDIF
        CALL EZRSET
      ENDIF
      IF(SKIPS_DONE.GE.NUMBER_OF_SKIPS) GOTO 999
C
C      if no event ids given
      IF(EVENTS_TO_SKIP(1,1).LE.0) THEN
        IF(EVTCNT().LE.NUMBER_OF_SKIPS) SKIP_D0RECO=.TRUE.
C
      ELSE                   ! check if event in range to skip
        CALL EVNTID(RUN,ID)
        NSKIP=SKIPS_DONE+1
        IF(RUN.GE.EVENTS_TO_SKIP(1,NSKIP).AND.
     &    ID.GE.EVENTS_TO_SKIP(2,NSKIP)) SKIP_D0RECO=.TRUE.
        IF(RUN.GE.EVENTS_TO_SKIP(3,NSKIP).AND.
     &    ID.GT.EVENTS_TO_SKIP(4,NSKIP)) THEN
          SKIPS_DONE=NSKIP
          SKIP_D0RECO=.FALSE.
        ENDIF
      ENDIF
  999 RETURN
  100 FORMAT(' Number of skips requested exceeds maximum allowed (',I3,
     &  ')')
      END
