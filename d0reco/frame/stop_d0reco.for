      FUNCTION STOP_D0RECO()
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : 
C-     Stop processing if requested events or runs done
C-   
C-   Outputs : true if conditions for stopping met
C-
C-   Created  14-SEP-1989   Serban D. Protopopescu
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
      LOGICAL STOP_D0RECO
      INTEGER NUMBER_OF_EVENTS,EVTCNT,IER
      LOGICAL FIRST
      DATA FIRST/.TRUE./
C----------------------------------------------------------------------
C
      STOP_D0RECO=.FALSE.
      IF(FIRST) THEN
        CALL EZPICK('D0RECO_RCP')
        CALL EZGET('NUMBER_OF_EVENTS',NUMBER_OF_EVENTS,IER)
        CALL EZRSET
        FIRST=.FALSE.
      ENDIF
      IF(NUMBER_OF_EVENTS.LT.0) GOTO 999
      IF(NUMBER_OF_EVENTS.LT.EVTCNT()) STOP_D0RECO=.TRUE.
  999 RETURN
      END
