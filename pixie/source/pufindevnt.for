      SUBROUTINE PUFINDEVNT(GETEVENT)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Perform a sequential search for the event
C-   EVTCNT requested by the user. If the current event is not the
C-   requested one the flag GETEVENT is set TRUE to cause an exit from
C-   PIXIE so that the program frame can read in the next event.
C-
C-             EVTCNT - If the requested event number is found EVTCNT will be
C-                      returned with the values -1
C-                      If the event number requested is NOT found EVTCNT will
C-                      be set to 0 and the NEXT_EVENT flag will be turned off
C-
C-   Inputs  : None
C-
C-   Outputs : GETEVENT- Logical Flag that will signal to get another event.
C-
C-   Created  15-JUN-1990   Lupe Howell
C-   Updated   9-SEP-1990   Harrison B. Prosper
C-      Use EVONUM
C-   Updated   2-OCT-1990   Lupe Howell  Displaying Not found message
C-   Updated  23-SEP-1992   Jasbir Singh Selection of particular Run
C-                          and Event Number incorporated
C-   Updated   1-OCT-1992   Lupe Howell   Put a limit in the number of events
C-                          searched.
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      INCLUDE 'D0$INC:PXCOMK.INC'
C
      LOGICAL GETEVENT
C
      INTEGER RUN,RUNNO,IEVENT, EVONUM,L,M,N,MAXSEARCH,OLD_EVENT
      INTEGER TOT_SEARCH,IER
      CHARACTER*8 EVNT_SKIPED,RUN_SKIPED
      CHARACTER*80 MSSG,TEMP,STRING
      LOGICAL EZERROR
C
      CHARACTER*18 SKIPINGMSG
      DATA SKIPINGMSG/' SKIP RUN/EVENT > '/
C
      SAVE OLD_EVENT,TOT_SEARCH
      DATA OLD_EVENT /0/
C----------------------------------------------------------------------
      GETEVENT = .FALSE.
C
C ****  Initialize search counter.
C
      IF ( OLD_EVENT .NE. EVTCNT ) THEN
        TOT_SEARCH = 0
        OLD_EVENT = EVTCNT
      ENDIF
C
C ****  Getting the system parameter MAXSEARCH
C
      CALL EZPICK('PX_SYSTEM_RCP')
      IF( EZERROR(IER) ) THEN
        CALL ERRMSG('PIXIE','PUFINDEVNT',
     &                  'Can not find PX_SYSTEM_RCP','W')
        MAXSEARCH = 200
      ELSE
        CALL PUGETV('MAXSEARCH',MAXSEARCH)
        CALL EZRSET
      ENDIF
C
C ****  Get the event number of the current event
C
      RUN = RUNNO()
      IEVENT = EVONUM()
C
C ****  Comapring RUNNO requested
C
      IF((RUN.EQ.RUNCNT) .OR. (RUNCNT .LE. 0))THEN       ! Match
        RUNCNT = -1
C
C ****  Comapring event requested
C
        IF((EVTCNT.EQ.IEVENT).OR.(EVTCNT.LE.0.))THEN       ! Match
          EVTCNT=-1
C
C ****  Continue to search until found or EOF...
C
        ELSE
          IF (RUNCNT .LE. 0) THEN
            CALL PXITOC(RUN,8,RUN_SKIPED)
            CALL PXITOC(IEVENT,8,EVNT_SKIPED)
            MSSG=SKIPINGMSG//RUN_SKIPED//EVNT_SKIPED
            CALL INTMSG(MSSG)
          ENDIF
          GETEVENT =.TRUE.
          TOT_SEARCH = TOT_SEARCH + 1
        ENDIF
        GO TO 800
      ELSE
        GETEVENT =.TRUE.
        TOT_SEARCH = TOT_SEARCH + 1
      ENDIF
C
C ****  Check if the number if events skipped had reach the
C ****  limit, if so ask the user if he/she wants to continue
C
  800 IF( ( TOT_SEARCH .GE. MAXSEARCH ).AND.( EVTCNT .GT. 0 ) )THEN
        CALL PXITOC(EVTCNT,10,EVNT_SKIPED)
        CALL WORD(EVNT_SKIPED,L,M,N)
        TEMP = ' Do you want to continue seaching for event '
     &      //EVNT_SKIPED(L:M)//'(Y/N)?'
        CALL GETPAR(1,TEMP,'U',STRING)
        IF ( STRING(1:1) .EQ. 'N' ) THEN
          TEMP = ' Search for event '//EVNT_SKIPED(L:M)//' STOPPED'
          CALL INTMSG(TEMP)
          EVTCNT=-1
        ENDIF
        TOT_SEARCH = 0
      ENDIF
  999 RETURN
      END
