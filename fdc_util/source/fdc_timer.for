      SUBROUTINE FDC_TIMER_UPDATE(STRING)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : Provide timing output to user.out.
C-      Only perfomed if FTRAKS_RCP switch, DBG_TIMER, is true.
C-      (and only done on the VAX).
C-
C-   Inputs  : STRING  To indcate where we are.
C-   Outputs : 
C-   Controls: 
C-
C-   Created  26-OCT-1993   Robert E. Avery
C-   Updated   7-DEC-1993   Robert E. Avery  Insert machine block.
C-
C----------------------------------------------------------------------
      IMPLICIT NONE
C
      CHARACTER*(*) STRING
C
      INTEGER RUN,EVENT,RUNSAV,EVENTSAV
      INTEGER LUNTIME,USUNIT
      INTEGER CPU_TIME, ELAPSED_TIME(2)
      INTEGER TIMER_ADDR,IER
      REAL    CPU_SEC, ELAPSED_SEC
      REAL    ACC_CPU_SEC, ACC_ELAPSED_SEC 
      LOGICAL FIRST 
      LOGICAL TIMER 
      DATA TIMER /.TRUE./
      DATA FIRST /.TRUE./
      DATA RUNSAV,EVENTSAV / -1, -1/
C----------------------------------------------------------------------
C
C
C&IF VAXVMS
      IF ( .NOT.TIMER ) GOTO 999
      IF ( FIRST ) THEN
        CALL EZPICK('FTRAKS_RCP')
        CALL EZGET('DBG_TIMER',TIMER,IER)
        CALL EZRSET
        IF ( .NOT.TIMER ) GOTO 999
C
        LUNTIME=USUNIT()
        TIMER_ADDR = 0
        CALL LIB$INIT_TIMER(TIMER_ADDR)
        FIRST = .FALSE.
      ENDIF
C
      CALL EVNTID(RUN,EVENT)
      IF ( RUN.NE.RUNSAV.OR.EVENT.NE.EVENTSAV ) THEN
        EVENTSAV = EVENT
        RUNSAV = RUN
        WRITE(LUNTIME,*) 
        WRITE(LUNTIME,*) string
        WRITE(LUNTIME,*) ' RUN:',RUN,' EVENT:',EVENT
        ACC_ELAPSED_SEC = 0.
        ACC_CPU_SEC = 0.
        CALL LIB$INIT_TIMER(TIMER_ADDR)
        GOTO 999
      ENDIF
C
      CALL LIB$STAT_TIMER(1,ELAPSED_TIME,TIMER_ADDR)
      CALL LIB$STAT_TIMER(2,CPU_TIME,TIMER_ADDR)
c
      ELAPSED_SEC = ABS(FLOAT(ELAPSED_TIME(1)))/(1.E7)
      CPU_SEC = FLOAT(CPU_TIME)/100.
c
      ACC_ELAPSED_SEC = ACC_ELAPSED_SEC + ELAPSED_SEC 
      ACC_CPU_SEC = ACC_CPU_SEC + CPU_SEC 
c
      WRITE(LUNTIME,*) string
      WRITE(LUNTIME,*) '  local  Elapsed time: ',ELAPSED_SEC 
      WRITE(LUNTIME,*) '  local  CPUtime:      ',CPU_SEC
      WRITE(LUNTIME,*) '  Accum. Elapsed time:             ',
     &                                          ACC_ELAPSED_SEC 
      WRITE(LUNTIME,*) '  Accum. CPU time:                 ',
     &                                          ACC_CPU_SEC
C
      CALL LIB$INIT_TIMER(TIMER_ADDR)
C
C&ENDIF
      RETURN
C
C----------------------------------------------------------------------
      ENTRY FDC_TIMER_RESET
C
C&IF VAXVMS
      IF ( .NOT.TIMER ) GOTO 999
      CALL LIB$INIT_TIMER(TIMER_ADDR)
C
C&ENDIF
  999 CONTINUE
      RETURN
      END
