      SUBROUTINE EVENTS
C------------------------------------------------------------------------------
C-                                                                
C-      Handle events after analysis                              
C-                                                                
C-   INPUT:                                                       
C-                                                                
C-   ENTRY SELEDS
C-     Preselect up to 20 events for display
C-   ENTRY ANALYZE_EVT
C-     Set flag to analyze only displayed events.
C-                                                                
C-     SDP Dec.,1986  NOV.,1988
C-   Updated  20-AUG-1991   Susan K. Blessing   Added option of displaying
C-    the first event of a run.  Added option of processing only events in
C-    event display list - ENTRY ANALYZE_EVT.  Fixed bug in displaying
C-    event list.
C-   Updated  10-Jan-1996  sss - compile with g77.
C-   
C------------------------------------------------------------------------------
      IMPLICIT NONE
C
      LOGICAL USRWRT
      LOGICAL FLGVAL,NOMORE
      LOGICAL OK,ASK,ASK1,PICK_EVTS
C
      INTEGER I,ELIST(20,2),ID,RUN,NL,IDONE(2)
      INTEGER ELIST2(20,2)
C
      SAVE ELIST,ELIST2,NL,PICK_EVTS
C
      DATA PICK_EVTS/.FALSE./
C-------------------------------------------------------------------
C
C        handle event display requests
C
      CALL EVNTID(RUN,ID)
C
      DO 11 I = 1, NL    ! check for SETUP display request
        IF (  (ELIST(I,1).EQ.RUN.AND.ELIST(I,2).EQ.ID)
     &    .OR.(ELIST(I,1).EQ.RUN.AND.ELIST(I,2).LT.0)
     &    .OR.(ELIST(I,1).LT.0)) THEN
          CALL PXMAIN(NOMORE)
          ELIST(I,1) = 0
          ELIST(I,2) = 0
          IF (.NOT.NOMORE) CALL FLGSET('EVENT_DISPLAY',.TRUE.)
          IDONE(1) = RUN
          IDONE(2) = ID
          GOTO 12
        ENDIF
   11 CONTINUE
   12 CONTINUE
C
      IF (IDONE(1).NE.RUN.OR.IDONE(2).NE.ID) THEN
        IF (FLGVAL('EVENT_DISPLAY')) THEN
          CALL PXMAIN(NOMORE)
          IF (NOMORE) CALL FLGSET('EVENT_DISPLAY',.FALSE.)
        ENDIF
      ENDIF
C
      CALL EVWRIT       ! single event write requests
C
      CALL DMPPRO       ! processed event dumps
C
      IF (USRWRT() ) THEN
        CALL USRZEV       ! zero event arrays if necessary
        CALL EVTWOS       ! write event to ouput streams
      ELSE
        CALL USRZEV       ! zero event arrays if necessary
      ENDIF
C
      RETURN
C
      ENTRY SELEDS
C
      ASK = .TRUE.
      ASK1 = .FALSE.
      PICK_EVTS = .FALSE.
C
   20 NL = 1
      CALL GETPAR(1,' How many events to display (<20)? [1] >',
     &   'I',NL)
      IF (NL.GT.20) GOTO 20
C
      DO 101 I = 1,NL
        ELIST(I,1) = -1
        ELIST(I,2) = -1
  101 CONTINUE
C
      CALL OUTMSG(' Give Run# and Event#.')
      CALL OUTMSG(' If Run#<0, first event read is displayed.')
      CALL OUTMSG(' If Event#<0, first event of Run# is displayed.')
      CALL OUTMSG(
     &  ' To display consecutive events use the Next Event feature')
      CALL OUTMSG(' in the Event Display menu.')
C
      DO 102 I = 1,NL
        CALL GETPAR(1,' Run no.>','I',ELIST(I,1))
        CALL GETPAR(1,' Event no.>','I',ELIST(I,2))
        IF (ELIST(I,1).LT.0) THEN
          ASK = .FALSE.
          GOTO 103
        END IF
        IF (ELIST(I,2).LT.0) ASK1 = .TRUE.
  102 CONTINUE
C
  103 CONTINUE
C
      IF (ASK) THEN
        IF (ASK1) THEN
          CALL GETPAR(1,' Do you want to process events in this(these)
     & run(s) only? Y/N >','L',PICK_EVTS)
        ELSE
          CALL GETPAR(1,
     &      ' Do you want to process only these events? Y/N >',
     &      'L',PICK_EVTS)
        END IF
        IF (PICK_EVTS) THEN
          DO I = 1, NL
            ELIST2(I,1) = ELIST(I,1)
            ELIST2(I,2) = ELIST(I,2)
          END DO
        END IF
      END IF
C
      RETURN
C
      ENTRY ANALYZE_EVT(OK)
C
      OK = .TRUE.
C
      IF (PICK_EVTS) THEN
        OK = .FALSE.
        CALL EVNTID(RUN,ID)
C
        DO I = 1, NL
          IF (  (ELIST2(I,1).EQ.RUN.AND.ELIST2(I,2).EQ.ID)
     &      .OR.(ELIST2(I,1).EQ.RUN.AND.ELIST2(I,2).LT.0)) THEN
            OK = .TRUE.
            GOTO 100
          END IF
        END DO
      ENDIF
  100 CONTINUE
C
  999 RETURN
      END
